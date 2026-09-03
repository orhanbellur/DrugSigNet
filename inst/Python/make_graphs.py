 # Importing graph_tool,pandas library for graph creation and manipulation
import graph_tool.all as gt
import pandas as pd

def _as_df(data, sheet_name=None):
    if isinstance(data, str):
        return pd.read_excel(data, sheet_name=sheet_name)
    return pd.DataFrame(data)


def create_gt(drug_node_edge, gene_node_edge, drug_node, gene_node, file_name):
    """
    Create a graph tool file with all genes and drugs from the provided datasets.

    Parameters:
    - drug_node_edge: Path to the Excel file containing edges connecting drugs to genes.
    - gene_node_edge: Path to the Excel file containing edges connecting genes to genes.
    - drug_node: Path to the Excel file containing drug nodes.
    - gene_node: Path to the Excel file containing gene nodes.
    - file_name: The name of the output file where the graph will be saved (without extension).
    
    Returns:
    - g: The constructed graph object.
    - vertices: A dictionary mapping gene graph IDs to their corresponding vertex indices.
    """
    # Load the input data from Excel files or in-memory data frames
    gene_node = _as_df(gene_node, sheet_name="gene_nodes")
    drug_node = _as_df(drug_node, sheet_name="drug_nodes")
    drug_node_edge = _as_df(drug_node_edge, sheet_name="drug_edges")
    gene_node_edge = _as_df(gene_node_edge, sheet_name="gene_edges")
    
    # Create an undirected graph
    g = gt.Graph(directed=False)
    
    # Create properties for edges
    e_type = g.new_edge_property("string")  # Property to store type of edge (e.g., drug-gene, gene-gene)
    e_graphId = g.new_edge_property("string")  # Property to store graph ID associated with the edge

    # Create properties for vertices
    v_type = g.new_vertex_property("string")  # Property to store type of vertex (Node or Drug)
    v_name = g.new_vertex_property("string")  # Property to store name of the vertex (e.g., gene name, drug name)
    v_graphId = g.new_vertex_property("string")  # Property to store graph ID associated with the vertex
    v_geneID = g.new_vertex_property("string")  # Property to store the gene ID
    v_status = g.new_vertex_property("string")  # Property for drug status (e.g., approved, experimental)

    # Associate properties with the graph
    g.edge_properties["type"] = e_type
    g.edge_properties["graphId"] = e_graphId
    g.vertex_properties["type"] = v_type
    g.vertex_properties["name"] = v_name
    g.vertex_properties["graphId"] = v_graphId
    g.vertex_properties["geneID"] = v_geneID  # Changed to geneID
    g.vertex_properties["status"] = v_status

    # Initialize dictionaries to hold vertex indices for connecting edges
    vertices = {}
    drug_vertices = {}

    # Add gene vertices to the graph
    print("adding nodes")
    for index, gene in gene_node.iterrows():
        v = g.add_vertex()  # Add a new vertex for each gene
        v_type[v] = 'Node'  # Set vertex type as 'Node'
        v_name[v] = gene['gene']  # Set the name of the gene (can be changed to geneID if desired)
        v_graphId[v] = gene['gene_graph_id']  # Set the graph ID for the gene
        v_geneID[v] = gene['gene']  # Set the gene ID for the vertex

        # Store the index of the vertex under its corresponding gene graph ID
        if gene['gene_graph_id'] not in vertices:
            vertices[gene['gene_graph_id']] = [int(v)]
        else:
            vertices[gene['gene_graph_id']].append(int(v))

    # Add drug vertices to the graph
    print("adding drugs")
    for index, drug in drug_node.iterrows():
        v = g.add_vertex()  # Add a new vertex for each drug
        v_type[v] = 'Drug'  # Set vertex type as 'Drug'
        v_name[v] = drug['Drug']  # Set the name of the drug
        v_status[v] = drug['Group']  # Set the status of the drug
        v_graphId[v] = drug['drug_graph_id']  # Set the graph ID for the drug
        drug_vertices[drug['drug_graph_id']] = int(v)  # Store the index of the drug vertex
    print("done with drugs")

    # Add edges between gene vertices based on gene edges data
    print("adding edges")
    for index, gene_edge in gene_node_edge.iterrows():
        a_indices = vertices[gene_edge['g1_graph_id']]  # Get indices of vertices for the first gene
        b_indices = vertices[gene_edge['g2_graph_id']]  # Get indices of vertices for the second gene

        done = set()  # To avoid duplicate edges
        for a in a_indices:
            for b in b_indices:
                if (a, b) and (b, a) not in done:  # Check if edge already added
                    e = g.add_edge(a, b)  # Add the edge between two gene vertices
                    e_type[e] = 'gene-gene'  # Set the edge type
                    e_graphId[e] = gene_edge['graphId']  # Set the edge's graph ID
                    done.add((a, b))  # Mark edge as added
                    done.add((b, a))  # Mark reverse edge as added
    print("done with edges")

    # Add edges between drug vertices and gene vertices based on drug-gene edges data
    print("adding drug edges")
    for index, drug_gene in drug_node_edge.iterrows():
        genes, drug = vertices[drug_gene['gene_graph_id']], drug_vertices[drug_gene['drug_graph_id']]
        for gene in genes:
            e = g.add_edge(drug, gene)  # Add an edge between the drug vertex and each connected gene vertex
            e_type[e] = 'drug-gene'  # Set the edge type
            e_graphId[e] = drug_gene['graphId']  # Set the edge's graph ID

    print("done with drug edges")
    file_name = file_name + '.gt'  # Append file extension for the output file
    g.save(file_name)  # Save the graph to the specified file

    return g, vertices  # Return the graph and the vertices dictionary
