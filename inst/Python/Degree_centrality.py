import sys
import os
import numpy as np
import pandas as pd
from tqdm import tqdm

import graph_tool as gt
import graph_tool.topology as gtt

from read_graph_tool_graph import read_graph_tool_graph
from scores_to_results import scores_to_results
from edge_weights import edge_weights  # If weighted distances are desired

# Get the path to the inst/Python directory of the package
package_dir = os.path.dirname(os.path.abspath(__file__))  # Get current script directory
python_scripts_dir = os.path.join(package_dir, 'Python')  # Point to 'inst/Python' directory

# Add the path to sys.path to allow imports
sys.path.append(python_scripts_dir)


#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Weighted Multi-Hop Diffusion Centrality (with 1–2–3 hop penalties)
Author: Orhan (with 3-step extension)
"""

def degree_centrality(file_path, file_name, seeds, max_deg, ignored_edge_types,
                      include_indirect_drugs, include_non_approved_drugs,
                      filter_paths, result_size, target='drug'):

    with tqdm(total=100, desc="Degree Centrality Pipeline", unit="step") as pbar:

        # Step 1: Load graph
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
        pbar.update(30)

        # -----------------------------
        # Step 2: Multi-hop centrality
        # -----------------------------
        pbar.set_description("Calculating 1–2 step penalized centrality")

        
        # Create score vector
        scores = g.new_vertex_property("float")

        # Convert seeds to a set for fast exclusion
        seed_set = set(seed_graph_ids)

        for seed in tqdm(seed_graph_ids, desc="Processing seed nodes", leave=False):

            # --- 1-step neighbors ---
            one_step = set(g.get_all_neighbors(seed))
            for nb in one_step:
                if nb not in seed_set:
                    scores[nb] += 1

            # --- 2-step neighbors ---
            two_step = set()
            for nb in one_step:
                for nb2 in g.get_all_neighbors(nb):

                    if nb2 in seed_set:
                        continue
                    if nb2 in one_step:
                        continue

                    two_step.add(nb2)

            for nb2 in two_step:
                scores[nb2] += 0.5
            

        pbar.update(40)

        # Step 3: Convert to results
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
        pbar.update(20)

        # Step 4: Optionally save results
        pbar.set_description("Finalizing results")
        merged_list = list(zip(res['traces_degree']['Drug']['names'], res['traces_degree']['Drug']['y']))
        merged_list_df = pd.DataFrame(merged_list, columns=['Drug', 'Score'])
        if file_name:
            merged_list_df.to_excel(file_name, index=False, header=True)
        pbar.update(10)

    return res, merged_list_df
