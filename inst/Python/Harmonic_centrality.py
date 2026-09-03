#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct 12 16:25:15 2022

@author: orhan
"""
import sys
import os

# Get the path to the inst/Python directory of the package
package_dir = os.path.dirname(os.path.abspath(__file__))  # Get current script directory
python_scripts_dir = os.path.join(package_dir, 'Python')  # Point to 'inst/Python' directory

# Add the path to the sys.path to allow imports
sys.path.append(python_scripts_dir)



from tqdm import tqdm
from read_graph_tool_graph import read_graph_tool_graph
from scores_to_results import scores_to_results
from edge_weights import edge_weights

import graph_tool as gt
import graph_tool.topology as gtt
import numpy as np
import pandas as pd

def harmonic_centrality(file_path, file_name, seeds, max_deg, ignored_edge_types,
                         include_indirect_drugs, include_non_approved_drugs, filter_paths,
                         result_size, hub_penalty, target='drug'):
    """
    Computes the harmonic centrality of drugs in the provided graph.

    Parameters
    ----------
    file_path : str
        The path to the graph file.
    file_name : str
        The name of the output file to save results.
    seeds : list of str
        Seed node identifiers.
    max_deg : int
        Maximum degree for node filtering.
    ignored_edge_types : list of str
        Edge types to ignore during analysis.
    include_indirect_drugs : bool
        Whether to include indirect drugs.
    include_non_approved_drugs : bool
        Whether to include non-approved drugs.
    filter_paths : bool
        Whether to filter paths based on certain criteria.
    result_size : int
        The number of top results to return.
    hub_penalty : float
        The hub penalty value for edge weights.
    target : str, optional
        The target type for filtering ('drug' by default).

    Returns
    -------
    res : dict
        The results containing network and node attributes.
    merged_list_df : pd.DataFrame
        DataFrame containing drug scores.
    """

    with tqdm(total=100, desc="Harmonic Centrality Pipeline", unit="step") as pbar:
        # Step 1: Read and process the graph
        pbar.set_description("Loading graph")
        g, seed_graph_ids, drug_ids, degrees = read_graph_tool_graph(
            file_path=file_path,
            ignored_edge_types=ignored_edge_types,
            seeds=seeds,
            max_deg=max_deg,
            include_indirect_drugs=include_indirect_drugs,
            include_non_approved_drugs=include_non_approved_drugs,
            target=target
        )
        pbar.update(20)

        # Step 2: Calculate edge weights
        pbar.set_description("Calculating edge weights")
        weights = edge_weights(
            g,
            hub_penalty,
            inverse=True
        )
        pbar.update(20)

        # Step 3: Compute harmonic centrality scores
        pbar.set_description("Computing harmonic centrality")
        all_dists = []
        for node in tqdm(seed_graph_ids, desc="Processing seeds", leave=False):
            shortest_distances = gtt.shortest_distance(g, node, weights=weights).get_array()
            shortest_distances[shortest_distances == np.inf] = 999999999
            shortest_distances = np.array([len(seeds) / d for d in shortest_distances])
            all_dists.append(shortest_distances)
        scores = sum(all_dists)
        pbar.update(40)

        # Step 4: Transform scores to results format
        pbar.set_description("Formatting results")
        res = scores_to_results(
            target,
            result_size,
            g,
            seed_graph_ids,
            drug_ids,
            scores,
            filter_paths,
            degrees
        )
        pbar.update(10)

        # Step 5: Optionally save results to Excel
        pbar.set_description("Finalizing results")
        merged_list = list(zip(res['traces_degree']['Drug']['names'], res['traces_degree']['Drug']['y']))
        merged_list_df = pd.DataFrame(merged_list, columns=['Drug', 'Score'])
        if file_name:
            merged_list_df.to_excel(file_name, index=False, header=True)
        pbar.update(10)

    return res, merged_list_df
