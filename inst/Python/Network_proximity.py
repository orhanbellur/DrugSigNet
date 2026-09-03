#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov 22 15:02:41 2023

Author: orhan.bellur
"""

import sys
import os

# Get the path to the inst/Python directory of the package
package_dir = os.path.dirname(os.path.abspath(__file__))  # Get current script directory
python_scripts_dir = os.path.join(package_dir, 'Python')  # Point to 'inst/Python' directory

# Add the path to the sys.path to allow imports
sys.path.append(python_scripts_dir)



import pandas as pd
import networkx as nx
import numpy as np
import random
from joblib import Parallel, delayed
import logging
from tqdm import tqdm
import time
from copy import deepcopy
import multiprocessing

# Set multiprocessing start method for clean process isolation
multiprocessing.set_start_method("spawn", force=True)

# Configure logging
logging.basicConfig(level=logging.INFO, format="%(asctime)s - %(levelname)s - %(message)s")


# Utility functions
def parse_drug_target(drug_edges_df):
    """Parses drug target data frame into a dictionary of drug to target genes."""
    df = pd.DataFrame(drug_edges_df)
    data = zip(df['ID'].values, df['Target'].values)
    drug2target = {}
    for drug, target in data:
        try:
            drug2target.setdefault(drug, set()).add(target)
        except ValueError:
            continue
    return drug2target


def seed_gordon(seed_genes):
    """Converts incoming seed genes to a stripped set."""
    return set([str(g).strip() for g in seed_genes if g is not None])


def calculate_closest_distance(network, nodes_from, nodes_to, lengths=None):
    """Calculates the closest distance between two sets of nodes in a network."""
    values_outer = []
    if lengths is None:
        for node_from in nodes_from:
            values = []
            for node_to in nodes_to:
                try:
                    val = nx.shortest_path_length(network, node_from, node_to)
                    values.append(val)
                except nx.NetworkXNoPath:
                    continue
            d = min(values) if values else float('inf')
            values_outer.append(d)
    d = np.mean(values_outer)
    return d


def get_degree_binning(g, bin_size, lengths=None):
    """Groups nodes by degree into bins."""
    degree_to_nodes = {}
    for node, degree in g.degree():
        if lengths is not None and node not in lengths:
            continue
        degree_to_nodes.setdefault(degree, []).append(node)
    values = sorted(degree_to_nodes.keys())
    bins = []
    i = 0
    while i < len(values):
        low = values[i]
        val = degree_to_nodes[values[i]]
        while len(val) < bin_size:
            i += 1
            if i == len(values):
                break
            val.extend(degree_to_nodes[values[i]])
        if i == len(values):
            i -= 1
        high = values[i]
        i += 1
        if len(val) < bin_size and bins:
            low_, high_, val_ = bins[-1]
            bins[-1] = (low_, high, val_ + val)
        else:
            bins.append((low, high, val))
    return bins


def pick_random_nodes_matching_selected(network, bins, nodes_selected, n_random, degree_aware=True, seed=452456):
    """Picks random nodes matching the degree distribution of selected nodes."""
    if seed is not None:
        seed = int(seed)
        random.seed(seed)
    values = []
    for _ in range(n_random):
        if degree_aware:
            nodes_random = set()
            node_to_equivalent_nodes = get_degree_equivalents(nodes_selected, bins, network)
            for node, equivalent_nodes in node_to_equivalent_nodes.items():
                chosen = random.choice(equivalent_nodes)
                for _ in range(20):  # Try to find a distinct node (at most 20 times)
                    if chosen in nodes_random:
                        chosen = random.choice(equivalent_nodes)
                nodes_random.add(chosen)
            values.append(list(nodes_random))
        else:
            values.append(random.sample(network.nodes, len(nodes_selected)))
    return values


def get_degree_equivalents(seeds, bins, g):
    """Finds nodes with equivalent degree distribution."""
    seed_to_nodes = {}
    for seed in seeds:
        d = g.degree(seed)
        for l, h, nodes in bins:
            if l <= d and h >= d:
                mod_nodes = list(nodes)
                mod_nodes.remove(seed)
                seed_to_nodes[seed] = mod_nodes
                break
    return seed_to_nodes


def get_random_nodes(nodes, network, bins=None, n_random=1000, min_bin_size=100, degree_aware=True, seed=452456):
    """Generates random nodes matching the degree distribution of the given nodes."""
    if bins is None:
        bins = get_degree_binning(network, min_bin_size)
    return pick_random_nodes_matching_selected(network, bins, nodes, n_random, degree_aware, seed=seed)


def calculate_proximity(network, nodes_from, nodes_to, n_random=1000, min_bin_size=100, seed=452456):
    """Calculates proximity between two sets of nodes."""
    nodes_network = set(network.nodes())
    nodes_from = set(nodes_from) & nodes_network
    nodes_to = set(nodes_to) & nodes_network
    if len(nodes_from) == 0 or len(nodes_to) == 0:
        return None
    d = calculate_closest_distance(network, nodes_from, nodes_to)
    bins = get_degree_binning(network, min_bin_size)
    nodes_from_random = get_random_nodes(nodes_from, network, bins=bins, n_random=n_random, min_bin_size=min_bin_size, seed=seed)
    nodes_to_random = get_random_nodes(nodes_to, network, bins=bins, n_random=n_random, min_bin_size=min_bin_size, seed=seed)
    random_values_list = zip(nodes_from_random, nodes_to_random)
    values = np.array([calculate_closest_distance(network, r_from, r_to) for r_from, r_to in random_values_list])
    m, s = np.mean(values), np.std(values)
    z = (d - m) / s if s != 0 else 0.0
    return d, z, (m, s)


def single_proximity(sample):
    """Calculates proximity for a single drug-disease pair."""
    try:
        drug_key, disease_key, drug_targets, disease_genes, network, nsims = sample
        nodes_from = set(drug_targets[drug_key]) & set(network.nodes())
        nodes_to = set(disease_genes[disease_key]) & set(network.nodes())
        if len(nodes_from) == 0 or len(nodes_to) == 0:
            return
        d, z, (mean, sd) = calculate_proximity(network, nodes_from, nodes_to, n_random=nsims)
        return [drug_key, disease_key, d, z, mean, sd]
    except Exception as e:
        return {'Drug': drug_key, 'Disease': disease_key, 'Error': str(e)}


def read_network(gene_edges_df):
    """Builds a graph from a gene-edge data frame."""
    df = pd.DataFrame(gene_edges_df).iloc[:, [0, 1]]
    G = nx.Graph()
    for _, row in df.iterrows():
        G.add_edge(row.iloc[0], row.iloc[1])
    return G


# Main function
def Network_Proximity(disease_genes, drug_edges_df, gene_edges_df, nsims, ncpus, start=0, end=None, random_seed=42):
    # Set global seed for reproducibility
    random_seed=int(random_seed)
    random.seed(random_seed)
    np.random.seed(random_seed)

    # Read inputs
    disease_gene_set = seed_gordon(disease_genes)
    drug_targets = parse_drug_target(drug_edges_df)
    G = read_network(gene_edges_df)

    # Subgraph (or skip if debugging)
    largest_cc = max(nx.connected_components(G), key=len)
    G1 = G.subgraph(sorted(largest_cc)).copy()  # Sort for deterministic subgraph

    # Prepare disease genes dictionary
    disease_genes = {"disease_genes": disease_gene_set & set(G1.nodes())}

    # Prepare samples
    samples = [
        [drug, disease, drug_targets, disease_genes, G1, nsims]
        for drug in drug_targets.keys()
        for disease in disease_genes.keys()
    ]

    # Apply start and end slicing
    if end is None:
        end = len(samples)
    samples = samples[start:end]

    # Parallel wrapper with worker-specific seeds
    def wrapper(sample, worker_seed):
        worker_seed = int(worker_seed)
        random.seed(worker_seed)
        np.random.seed(worker_seed)
        sample[2] = deepcopy(sample[2])  # Ensure isolation of shared objects
        sample[3] = deepcopy(sample[3])
        sample[4] = deepcopy(sample[4])
        return single_proximity(sample)

    # Generate unique, deterministic seeds for each sample
    worker_seeds = np.arange(start, start + len(samples)) + random_seed

    # Run parallel jobs with assigned seeds
    res = Parallel(n_jobs=ncpus)(
        delayed(wrapper)(sample, worker_seeds[i]) for i, sample in enumerate(samples)
    )

    # Compile results
    results = pd.DataFrame([r for r in res if isinstance(r, list)], columns=['Drug', 'Disease', 'D', 'Z', 'Mean', 'SD'])
    
    return results

