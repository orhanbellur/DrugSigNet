#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Oct  5 23:10:39 2022

@author: orhan
"""

import graph_tool.stats as gts

SMALL_VALUE = 1 * 10**-10

def _calc_score(g, e, label, graph_key):
    """Calculate the source and target scores for a given edge based on labels.

    Args:
        g (graph_tool.Graph): The graph containing vertices and edges.
        e (graph_tool.Edge): The edge for which scores are calculated.
        label (str or None): The label used to calculate scores.
        graph_key (str): The key for accessing vertex properties.

    Returns:
        tuple: A tuple containing the source and target scores.
    """
    if label is not None:
        # Obtain the score from vertex properties based on the label.
        source = 1 if g.vertex_properties["type"][int(e.source())] == 'Drug' else \
            g.vertex_properties[graph_key][int(e.source())][label.name]
        target = 1 if g.vertex_properties["type"][int(e.target())] == 'Drug' else \
            g.vertex_properties[graph_key][int(e.target())][label.name]

        # Normalize scores, using SMALL_VALUE to avoid zero.
        source = source if source is not None and source != 0.0 else SMALL_VALUE
        target = target if target is not None and target != 0.0 else SMALL_VALUE
    else:
        # Default scores if no label is provided.
        source, target = 0.5, 0.5
    
    return source, target

def _calc_hub_penalty(g, hub_penalty, avdeg, weights, inverse):
    hub_penalty_weights = {}
    for e in g.edges():
        edge_avdeg = float(e.source().out_degree() + e.target().out_degree()) / 2.0
        penalized_weight = (1.0 - hub_penalty) * avdeg + hub_penalty * edge_avdeg
        if inverse:
            hub_penalty_weights[e] = 1.0 / penalized_weight
        else:
            hub_penalty_weights[e] = penalized_weight
    # normalize hub penalty weights
    m = max(hub_penalty_weights.values())
    hub_penalty_weights_list = [float(i)/m for i in hub_penalty_weights.values()]
    # update weights
    for i, e in enumerate(hub_penalty_weights):
        weights[e] *= hub_penalty_weights_list[i]
    return weights

def edge_weights(g, hub_penalty, inverse=False):
    """Calculate edge weights for a graph with an optional hub penalty.

    Args:
        g (graph_tool.Graph): The graph to calculate weights for.
        hub_penalty (float): Penalty for hubs (0 = no penalty, 1 = maximum penalty).
        inverse (bool, optional): If True, applies inverse weighting. Defaults to False.

    Raises:
        ValueError: If the hub penalty is outside the range [0, 1].

    Returns:
        graph_tool.EdgeProperty: The computed edge weights for the graph.
    """
    avdeg = gts.vertex_average(g, "total")[0]  # Calculate average vertex degree.
    weights = g.new_edge_property("double", val=avdeg)  # Initialize weights to average degree.
    
    if hub_penalty < 0 or hub_penalty > 1:
        raise ValueError("Invalid hub penalty {}. Must be in range [0, 1].".format(hub_penalty))

    if hub_penalty > 0:
        # Apply hub penalty if specified.
        weights = _calc_hub_penalty(g, hub_penalty, avdeg, weights, inverse)
    
    return weights
