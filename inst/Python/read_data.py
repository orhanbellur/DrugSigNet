def drug_network(file_name):
    """Reads a drug-edge Excel file and creates mappings from drug IDs to Ensembl target IDs and drug names."""

    import pandas as pd
    
    #path_par = os.path.abspath(os.path.join(path_cur, os.pardir))
   
    df = pd.read_excel(file_name,sheet_name='drug_edges')
    data= zip(df['ID'].values, df['Target'].values)
    data_names = zip(df['ID'].values, df['Drug'].values)
    drug2target={}
    for drug, Target in data:
        try:
            drug2target.setdefault(drug, set()).add(Target)
        except ValueError: continue

    drug2name = {}
    for drug, Drug in data_names:
        try:
            drug2name.setdefault(drug, set()).add(str(Drug).lower())
        except ValueError:
            continue

    results = {'drug2target': drug2target, 'drug2name':drug2name}

    return results




def seed_gordon(file_name):
    """Loads gene identifiers from a CSV file into a set."""
    import pandas as pd
    genes = list(pd.read_csv(file_name).iloc[:, 0].str.strip().values)
    return set(genes)


def connected_component_subgraphs(G, copy=True):
    """Yields connected components of a graph as subgraphs."""
    import networkx as nx
    for c in nx.connected_components(G):
        if copy:
            yield G.subgraph(c).copy()
        else:
            yield G.subgraph(c)

def PPI(file_name):
    """Constructs a PPI graph from an edge file and returns the largest connected component."""
    import networkx as nx
    import pandas as pd
    file = pd.read_excel(file_name,sheet_name='gene_edges')
    rows = file.values.tolist()
   
    G_o = nx.Graph()

    for row in rows:

        G_o.add_node(row[0])
        G_o.add_node(row[1])
        G_o.add_edge(row[0], row[1])

    clusters = sorted(list(connected_component_subgraphs(G_o)), key=len, reverse=True)
    G = clusters[0]
    G.remove_edges_from(nx.selfloop_edges(G))

    return G


def clinical_trials(clinical_drug_file, drug_network_file):
    """Maps clinical drug IDs to their target IDs using a drug network."""
    import pandas as pd
    df = pd.read_excel(clinical_drug_file)
    drugs = set(df['ID'].values)
    drug_results = drug_network(drug_network_file)
    drug2targets = drug_results['drug2target']

    drug2targets_maped = {}
    for drug in drugs:
        drug2targets_maped[drug]= drug2targets[drug]

    return drug2targets_maped
