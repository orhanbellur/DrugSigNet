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
import graph_tool.centrality as gtc
import os.path
import pandas as pd


def trust_rank(file_path, file_name, seeds, max_deg, ignored_edge_types,
               include_indirect_drugs,
               include_non_approved_drugs, filter_paths,
               result_size, hub_penalty, damping_factor, target='drug'):
    
    # Progress Bar Initialization
    with tqdm(total=100, desc="TrustRank Pipeline", unit="step") as pbar:

        # Step 1: Load Graph
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
        pbar.update(20)  # Approximate progress percentage

        # Step 2: Compute Edge Weights
        pbar.set_description("Computing edge weights")
        weights = edge_weights(
            g,
            hub_penalty,
            inverse=True,
        )
        pbar.update(20)

        # Step 3: Trust Initialization
        pbar.set_description("Initializing trust vector")
        trust = g.new_vertex_property("double")
        trust.a[seed_graph_ids] = 1.0 / len(seed_graph_ids)
        pbar.update(10)

        # Step 4: Run PageRank
        pbar.set_description("Running PageRank")
        scores = gtc.pagerank(g, damping=damping_factor, pers=trust, weight=weights)
        pbar.update(20)

        # Step 5: Process Results
        pbar.set_description("Processing results")
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

        # Step 6: Optionally export results to Excel
        pbar.set_description("Finalizing results")
        merged_list = list(zip(res['traces_degree']['Drug']['names'], res['traces_degree']['Drug']['y']))
        merged_list_df = pd.DataFrame(merged_list, columns=['Drug', 'Score'])
        if file_name:
            merged_list_df.to_excel(file_name, index=False, header=True)
        pbar.update(10)

    return res, merged_list_df
