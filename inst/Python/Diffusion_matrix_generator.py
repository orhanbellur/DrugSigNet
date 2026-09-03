from __future__ import division
import numpy as np
import random
import networkx as nx
from scipy.spatial.distance import pdist, squareform
from scipy.stats import entropy
import pickle
from read_data import PPI

# Set the random seed for reproducibility
seed_value = 42  # Use a fixed seed value
np.random.seed(seed_value)
random.seed(seed_value)

# Function to calculate Kullback-Leibler Divergence
def kl_divergence(p, q):
    return np.sum(np.where(p != 0, p * np.log(p / q), 0))

# Function to calculate Jensen-Shannon divergence
def js_cal(p, q):
    m = (p + q) / 2
    return (entropy(p, m) + entropy(q, m)) / 2

# Function to create a KL divergence and JS divergence matrix
def kl_divergence_matrix(rw_matrix_in):
    rw = np.array(rw_matrix_in)
    rw_sum_rows = rw.sum(axis=1)
    rw = rw / rw_sum_rows[:, np.newaxis]
    
    n = rw_matrix_in.shape[0]
    rw_kl = np.zeros((n, n))
    rw_js = np.zeros((n, n))
    
    for i in range(n):
        for j in range(n):
            rw_kl[i, j] = kl_divergence(rw[i, :], rw[j, :])
            rw_js[i, j] = js_cal(rw[i, :], rw[j, :])
    
    results = {'RWKL': rw_kl, 'RWJS': rw_js}
    return results

# Function to generate a random walk and distance similarity matrix
def rw_dsd_generator(adjacency_in, nrw):
    adjacency = np.asmatrix(adjacency_in)
    n = adjacency.shape[0]
    degree = adjacency.sum(axis=1)
    p = adjacency / degree

    c = np.eye(n)
    for i in range(nrw):
        c = np.dot(c, p) + np.eye(n)
    
    dsd = squareform(pdist(c, metric='cityblock'))
    results = {'RW': c, 'DSD': dsd}
    return results

def process_ppi_network(ppi_file, num_random_walks=100):
    G = PPI(ppi_file)
    
    G_nodes = G.nodes()
    
    node_2_id = {node: idx for idx, node in enumerate(G_nodes)}
    id2node = {idx: node for idx, node in enumerate(G_nodes)}

    # Serialize the mappings
    id2node_path = f'{ppi_file}_id_2_node.p'
    node_2_id_path = f'{ppi_file}_node_2_id.p'
    pickle.dump(id2node, open(id2node_path, 'wb'))
    pickle.dump(node_2_id, open(node_2_id_path, 'wb'))

    # Convert the PPI graph to an adjacency matrix
    adjacency_ppi = np.array(nx.adjacency_matrix(G).todense())
    
    # Generate the random walk and distance similarity matrices
    results_rw_dsd = rw_dsd_generator(adjacency_ppi, num_random_walks)
    rw_matrix = results_rw_dsd['RW']
    dsd_matrix = results_rw_dsd['DSD']
    
    # Serialize the distance similarity matrix
    dsd_path = f'{ppi_file}_DSD_{num_random_walks}.p'
    pickle.dump(dsd_matrix, open(dsd_path, 'wb'))

    # Calculate KL and JS divergence matrices
    results_kl_js = kl_divergence_matrix(rw_matrix)
    kl_matrix = results_kl_js['RWKL']
    js_matrix = results_kl_js['RWJS']
    
    # Serialize the KL and JS divergence matrices
    kl_path = f'{ppi_file}_KL_{num_random_walks}.p'
    js_path = f'{ppi_file}_JS_{num_random_walks}.p'
    pickle.dump(kl_matrix, open(kl_path, 'wb'))
    pickle.dump(js_matrix, open(js_path, 'wb'))

    return {
        "id2node": id2node_path,
        "node2id": node_2_id_path,
        "dsd_matrix": dsd_path,
        "kl_matrix": kl_path,
        "js_matrix": js_path
    }
