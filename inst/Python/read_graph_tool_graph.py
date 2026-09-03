import graph_tool as gt

def read_graph_tool_graph(file_path, seeds, ignored_edge_types, max_deg, node_key='name',
                          include_indirect_drugs=False, 
                          include_non_approved_drugs=False, target='drug', available_drugs=None):
    """
    Reads a graph-tool graph from a file and processes it based on specified criteria.

    Parameters
    ----------
    file_path : str
        The path to a graphml or gt file containing the graph data.

    seeds : list of str
        A list of identifiers for the seed nodes (e.g., Entrez IDs or UniProt accessions).

    ignored_edge_types : list of str
        A list of edge types to ignore during analysis.

    max_deg : int
        The maximum degree threshold for node filtering.

    include_indirect_drugs : bool
        If True, edges from non-seed genes to drugs are considered.

    include_non_approved_drugs : bool
        If True, includes non-approved drugs in the analysis.

    target : str
        The target type for filtering ('drug' or otherwise).

    available_drugs : list of str or None
        A list of available drugs to filter out non-listed drugs.

    Returns
    -------
    g : graph_tool.Graph
        The processed graph.

    seed_ids : list of int
        Internal IDs of the seed nodes.

    drug_ids : list of int
        Internal IDs of the drugs.
    """
    # Load the graph from the specified file.
    g = gt.load_graph(file_path)

    # Convert seeds to a set for faster lookup.
    seeds_set = set(seeds)
    
    # Identify nodes to be deleted based on the defined criteria.
    deleted_nodes = []
    for node in range(g.num_vertices()):
        node_name = g.vertex_properties[node_key][node]
        node_type = g.vertex_properties["type"][node]
        
        # Check if the node should be deleted based on max degree and seed status.
        if node_name not in seeds_set and g.vertex(node).out_degree() > max_deg:
            deleted_nodes.append(node)
        elif target != 'drug' and node_type == "Drug":
            deleted_nodes.append(node)
        elif node_type == "Drug" and available_drugs is not None:
            if g.vertex_properties["name"][node].lower() not in available_drugs:
                deleted_nodes.append(node)

    # Remove the identified nodes from the graph.
    g.remove_vertex(deleted_nodes, fast=True)

    # Initialize lists to hold internal IDs of seeds and drugs.
    seed_ids = []
    drug_ids = []
    
    # Track which seeds were found in the graph.
    is_matched = {seed: False for seed in seeds_set}

    # Collect internal IDs of seed nodes and approved drugs.
    for node in range(g.num_vertices()):
        node_name = g.vertex_properties[node_key][node]
        node_type = g.vertex_properties["type"][node]
        
        if node_name in seeds_set:
            seed_ids.append(node)
            is_matched[node_name] = True
        
        if node_type == "Drug":
            status = g.vertex_properties["status"][node]
            if status == 'approved' or (include_non_approved_drugs and status == "unapproved"):
                drug_ids.append(node)

    # Ensure all seeds have been matched; raise error if not.
    for seed in seeds_set:
        if not is_matched[seed]:
            raise ValueError(f"Invalid seed node {seed}. No node named {seed} in {file_path}.")

    # Remove edges that are to be ignored.
    deleted_edges = []
    for edge in g.edges():
        edge_type = g.edge_properties["type"][edge]
        if edge_type in ignored_edge_types:
            deleted_edges.append(edge)
            continue

        # Remove drug-gene edges if not targeting drugs.
        if target != 'drug' and edge_type == "drug-gene":
            deleted_edges.append(edge)

    # Remove the identified edges from the graph.
    g.set_fast_edge_removal(fast=True)
    for edge in deleted_edges:
        g.remove_edge(edge)

    # Save degree information before filtering out indirect edges to drugs.
    degrees = {g.vertex_properties["graphId"][node]: g.vertex(node).out_degree() for node in range(g.num_vertices())}

    # If indirect drugs should not be included, filter those edges out.
    if not include_indirect_drugs:
        for edge in g.edges():
            if g.edge_properties["type"][edge] == "drug-gene" and \
               (edge.target() not in seed_ids and edge.source() not in seed_ids):
                g.remove_edge(edge)

    # Return the processed graph along with seed and drug IDs, and node degrees.
    return g, seed_ids, drug_ids, degrees
