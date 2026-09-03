#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct  5 22:12:17 2022

@author: orhan
"""

import graph_tool.topology as gtt

def scores_to_results(
        target,
        result_size,
        g,
        seed_ids,
        drug_ids,
        scores,
        filterPaths,
        degrees
):
    r"""Transforms the scores into the required result format, extracting relevant subgraphs and node attributes."""
    
    # Collect candidates based on the target type and scores.
    candidates = [(node, scores[node]) for node in drug_ids if scores[node] > 0] if target == "drug" else \
                 [(node, scores[node]) for node in range(g.num_vertices()) if scores[node] > 0 and node not in set(seed_ids)]
    
    # Select the top candidates based on score.
    best_candidates = [item[0] for item in sorted(candidates, key=lambda item: item[1], reverse=True)[:result_size]]

    returned_edges = set()
    returned_nodes = set(seed_ids)  # Always include seed IDs.

    # Process each candidate based on the filterPaths parameter.
    for candidate in best_candidates:
        for index, seed_id in enumerate(seed_ids):
            vertices, edges = gtt.shortest_path(g, candidate, seed_id)

            # Check if any drug is present in the path.
            drug_in_path = any(g.vertex_properties["type"][int(vertex)] == "Drug" and vertex != candidate for vertex in vertices)
            if drug_in_path:
                continue
            
            # Collect returned nodes and edges.
            returned_nodes.update(int(vertex) for vertex in vertices)
            returned_edges.update((edge.source(), edge.target()) for edge in edges)

    # Build the subgraph structure.
    subgraph = {
        "nodes": [g.vertex_properties["graphId"][node] for node in returned_nodes],
        "edges": [{"from": g.vertex_properties["graphId"][source], "to": g.vertex_properties["graphId"][target]} for source, target in returned_edges],
    }

    # Compute node attributes for the returned nodes.
    node_types = {g.vertex_properties["graphId"][node]: g.vertex_properties["type"][node] for node in returned_nodes}
    is_seed = {g.vertex_properties["graphId"][node]: node in set(seed_ids) for node in returned_nodes}
    returned_scores = {g.vertex_properties["graphId"][node]: scores[node] for node in returned_nodes}
    is_result = {g.vertex_properties["graphId"][node]: node in best_candidates for node in returned_nodes}
    db_degrees = {g.vertex_properties["graphId"][node]: degrees[g.vertex_properties["graphId"][node]] for node in returned_nodes}

    # Prepare data for traces of node degrees versus scores.
    traces_degree = {'Drug': {'x': [], 'y': [], 'names': []}, 'Node': {'x': [], 'y': [], 'names': []}}
    for node in returned_nodes:
        degree = degrees[g.vertex_properties["graphId"][node]]
        score = returned_scores[g.vertex_properties["graphId"][node]]
        node_type = node_types[g.vertex_properties["graphId"][node]]
        traces_degree[node_type]['x'].append(degree)
        traces_degree[node_type]['y'].append(score)
        traces_degree[node_type]['names'].append(g.vertex_properties["name"][node])

    return {
        "network": subgraph,
        "node_attributes": {
            "node_types": node_types,
            "is_seed": is_seed,
            "scores": returned_scores,
            "is_result": is_result,
            'db_degrees': db_degrees,
        },
        'traces_degree': traces_degree
    }
