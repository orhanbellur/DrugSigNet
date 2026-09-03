import pickle
import numpy as np
import os
import warnings
import pandas as pd  # Importing pandas for DataFrame operations
warnings.filterwarnings("ignore")
from read_data import clinical_trials  # Assuming these are your data import functions


def _load_node_2_id(path):
    """Load cached PPI node IDs and add string aliases for existing keys."""
    node_2_id = pickle.load(open(os.path.join(path, 'ppi_node_2_id.p'), 'rb'))
    return {**node_2_id, **{str(node).strip(): idx for node, idx in node_2_id.items()}}


def dsd_rank(seeds, set_drugs, path):
    """
    Calculate DSD (Distance Similarity Distance) ranks for given seeds and drugs.
    """
    # Load pre-computed mappings and DSD values
    node_2_id = _load_node_2_id(path)
    dsd_val = pickle.load(open(os.path.join(path, 'PPI_DSD_100.p'), 'rb'))

    node_2_prob_median = {}
    node_2_prob_min = {}
    
    for drug in set_drugs:
        prob_all_med = []
        prob_all_min = []

        for target in drug:
            prob_drug = []
            for s in seeds:
                if target == s: 
                    continue
                prob = dsd_val[node_2_id[target], node_2_id[s]]
                prob_drug.append(prob)

            med_prob = np.median(prob_drug) if prob_drug else 0
            min_prob = np.min(prob_drug) if prob_drug else 0
            prob_all_med.append(med_prob)
            prob_all_min.append(min_prob)

        node_2_prob_median[drug] = np.mean(prob_all_med) if prob_all_med else 0
        node_2_prob_min[drug] = np.mean(prob_all_min) if prob_all_min else 0

    return node_2_prob_median, node_2_prob_min


def kl_rank(seeds, set_drugs, path):
    """
    Calculate KL (Kullback-Leibler) ranks for given seeds and drugs.
    """
    # Load pre-computed mappings and KL values
    node_2_id = _load_node_2_id(path)
    kl_val = pickle.load(open(os.path.join(path, 'PPI_KL_100.p'), 'rb'))

    node_2_prob_median = {}
    node_2_prob_min = {}
    
    for drug in set_drugs:
        prob_all_med = []
        prob_all_min = []

        for target in drug:
            prob_drug = []
            for s in seeds:
                if target == s: 
                    continue
                prob = kl_val[node_2_id[target], node_2_id[s]]
                prob_drug.append(prob)

            med_prob = np.median(prob_drug) if prob_drug else 0
            min_prob = np.min(prob_drug) if prob_drug else 0
            prob_all_med.append(med_prob)
            prob_all_min.append(min_prob)

        node_2_prob_median[drug] = np.mean(prob_all_med) if prob_all_med else 0
        node_2_prob_min[drug] = np.mean(prob_all_min) if prob_all_min else 0

    return node_2_prob_median, node_2_prob_min


def js_rank(seeds, set_drugs, path):
    """
    Calculate JS (Jensen-Shannon) ranks for given seeds and drugs.
    """
    # Load pre-computed mappings and JS values
    node_2_id = _load_node_2_id(path)
    js_val = pickle.load(open(os.path.join(path, 'PPI_JS_100.p'), 'rb'))

    node_2_prob_median = {}
    node_2_prob_min = {}
    
    for drug in set_drugs:
        prob_all_med = []
        prob_all_min = []

        for target in drug:
            prob_drug = []
            for s in seeds:
                if target == s: 
                    continue
                prob = js_val[node_2_id[target], node_2_id[s]]
                prob_drug.append(prob)

            med_prob = np.median(prob_drug) if prob_drug else 0
            min_prob = np.min(prob_drug) if prob_drug else 0
            prob_all_med.append(med_prob)
            prob_all_min.append(min_prob)

        node_2_prob_median[drug] = np.mean(prob_all_med) if prob_all_med else 0
        node_2_prob_min[drug] = np.mean(prob_all_min) if prob_all_min else 0

    return node_2_prob_median, node_2_prob_min


class NetMeasure:
    """
    Class to calculate various network measures for drugs and their targets.
    """

    def __init__(self, G, seed_ids, drugs_to_targets, app_drugs_to_targets, path):
        # Initialize class variables and mappings
        self.path = path  # Path for loading pickle files
        net_nodes = set(G.nodes())
        drugs_list = list(drugs_to_targets.keys()) + list(app_drugs_to_targets.keys())

        targets_all = [frozenset(net_nodes & drugs_to_targets[drug]) for drug in drugs_to_targets.keys()] +\
                      [frozenset(net_nodes & app_drugs_to_targets[drug]) for drug in app_drugs_to_targets.keys()]

        self.drugs_all = drugs_list
        self.targets_all = targets_all
        self.app_drugs = [drug for drug in app_drugs_to_targets.keys()]

        targets_set_unique = set()
        for target in targets_all:
            targets_set_unique.add(target)

        self.net = G
        self.seeds = seed_ids & set(G.nodes())
        self.degree = list(map(lambda target: len(target), self.targets_all))

        self.dsd_min = None
        self.dsd_med = None
        self.kl_min = None
        self.kl_med = None
        self.js_min = None
        self.js_med = None

    def dsd(self):
        """
        Calculate DSD values and store them.
        """
        drug_2_prob_median, drug_2_prob_min = dsd_rank(self.seeds, self.targets_all, self.path)
        self.dsd_min = list(map(lambda drug: drug_2_prob_min[drug], self.targets_all))

        return self

    def kl(self):
        """
        Calculate KL values and store them.
        """
        drug_2_prob_median, drug_2_prob_min = kl_rank(self.seeds, self.targets_all, self.path)
        self.kl_med = list(map(lambda drug: drug_2_prob_median[drug], self.targets_all))
        self.kl_min = list(map(lambda drug: drug_2_prob_min[drug], self.targets_all))

        return self

    def js(self):
        """
        Calculate JS values and store them.
        """
        drug_2_prob_median, drug_2_prob_min = js_rank(self.seeds, self.targets_all, self.path)
        self.js_med = list(map(lambda drug: drug_2_prob_median[drug], self.targets_all))
        self.js_min = list(map(lambda drug: drug_2_prob_min[drug], self.targets_all))

        return self

    def make_df(self, ties_method='dense'):
        """
        Create a DataFrame with the calculated measures and ranks with specified ties_method.
        """
        dsd_res = self.dsd()
        kl_res = self.kl()
        js_res = self.js()

        data = {'Drug': self.drugs_all,
                'Degree': self.degree,
                'Targets': self.targets_all,
                'DSD-min': dsd_res.dsd_min,
                'KL-med': kl_res.kl_med,
                'KL-min': kl_res.kl_min,
                'JS-med': js_res.js_med,
                'JS-min': js_res.js_min}

        df = pd.DataFrame(data)

        models = ['DSD-min', 'KL-med', 'KL-min', 'JS-med', 'JS-min']

        # for m in models:
        #     df[m + '-Rank'] = df[m].rank(method=ties_method, ascending=True)
        #     df[m + '-Percentage'] = df[m].rank(method=ties_method, pct=True, ascending=True)
            
        for m in models:
          # Create a mask to exclude zeros
          mask = df[m] != 0
          # Rank only non-zero values
          df.loc[mask, m + '-Rank'] = df.loc[mask, m].rank(method=ties_method, ascending=True)
          df.loc[mask, m + '-Percentage'] = df.loc[mask, m].rank(method=ties_method, pct=True, ascending=True)

          # Optionally, set zero ranks/percentages to NaN (if you prefer them blank instead of ranked)
          df.loc[~mask, m + '-Rank'] = float('nan')
          df.loc[~mask, m + '-Percentage'] = float('nan')


        df['APP-Drugs'] = np.where(df['Drug'].isin(self.app_drugs), 'yes', 'no')
        df = df.drop_duplicates(subset='Drug', keep="first")
        df_final = df[['Drug', 'APP-Drugs', 'Degree', 'Targets'] + [m + '-Rank' for m in models] + [m for m in models]]

        return df_final


def _drug_network_from_df(drug_edges_df):
    df = pd.DataFrame(drug_edges_df)
    data = zip(df['ID'].values, df['Target'].values)
    drug2target = {}
    for drug, target in data:
        try:
            drug2target.setdefault(drug, set()).add(str(target).strip())
        except ValueError:
            continue
    return {'drug2target': drug2target}


def _seed_set(seed_genes):
    return set([str(g).strip() for g in seed_genes if g is not None])


def _ppi_from_df(gene_edges_df):
    import networkx as nx
    df = pd.DataFrame(gene_edges_df).iloc[:, [0, 1]]
    G_o = nx.Graph()
    for _, row in df.iterrows():
        node_a = str(row.iloc[0]).strip()
        node_b = str(row.iloc[1]).strip()
        G_o.add_node(node_a)
        G_o.add_node(node_b)
        G_o.add_edge(node_a, node_b)
    clusters = sorted(list(nx.connected_components(G_o)), key=len, reverse=True)
    G = G_o.subgraph(clusters[0]).copy()
    G.remove_edges_from(nx.selfloop_edges(G))
    return G


def Diff_run_pipelines(drug_edges_df, seed_genes, gene_edges_df, ties_method='dense', path='.'):
    """
    Run the pipeline for processing drug-target information and saving results.
    """
    Drug_network = _drug_network_from_df(drug_edges_df)
    drug2targets = Drug_network['drug2target']
    seed_ids = _seed_set(seed_genes)
    app_drugs = {}
    G = _ppi_from_df(gene_edges_df)
    network_status = NetMeasure(G, seed_ids, drug2targets, app_drugs, path)
    df_final = network_status.make_df(ties_method)

    return df_final
