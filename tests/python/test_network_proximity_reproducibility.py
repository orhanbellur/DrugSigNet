"""Regression tests for deterministic network proximity randomization.

Run from repository root:
    python -m unittest discover -s tests/python -p 'test_network_proximity_reproducibility.py'
Requires the package's Python dependencies (networkx, pandas, numpy, joblib).
"""
import json
import os
from pathlib import Path
import subprocess
import sys
import unittest

REPO_ROOT = Path(__file__).resolve().parents[2]
MODULE = REPO_ROOT / "inst" / "Python" / "Network_proximity.py"

RUN = r"""
import importlib.util
import json
import pandas as pd
import sys

spec = importlib.util.spec_from_file_location("network_proximity_under_test", sys.argv[1])
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

# Connected network with heterogeneous node degrees.
edges = [(f"G{i:03}", f"G{(i+1)%90:03}") for i in range(90)]
edges += [(f"G{i:03}", f"G{(i*7+23)%90:03}") for i in range(90)]
genes = pd.DataFrame(edges, columns=["gene1", "gene2"])
drug_edges = pd.DataFrame([
    ("drug_a", "G002"), ("drug_a", "G012"),
    ("drug_b", "G020"), ("drug_b", "G031"),
    ("drug_c", "G045"), ("drug_c", "G060"),
], columns=["ID", "Target"])
out = mod.Network_Proximity(
    disease_genes=["G005", "G024", "G044"],
    drug_edges_df=drug_edges,
    gene_edges_df=genes,
    nsims=30,
    ncpus=int(sys.argv[2]),
    random_seed=int(sys.argv[3]),
)
print("RESULT_JSON:" + out.sort_values("Drug").to_json(orient="records", double_precision=15))
"""

def run_proximity(hashseed, ncpus, seed):
    env = os.environ.copy()
    env["PYTHONHASHSEED"] = str(hashseed)
    result = subprocess.run(
        [sys.executable, "-c", RUN, str(MODULE), str(ncpus), str(seed)],
        cwd=str(REPO_ROOT), env=env, capture_output=True, text=True, check=True,
        timeout=120,
    )
    return json.loads(result.stdout.split("RESULT_JSON:")[-1].strip())


class TestNetworkProximityReproducibility(unittest.TestCase):
    def test_same_seed_independent_processes_and_hash_seeds(self):
        first = run_proximity(hashseed=1, ncpus=1, seed=42)
        self.assertEqual(first, run_proximity(hashseed=7, ncpus=1, seed=42))
        self.assertEqual(first, run_proximity(hashseed=101, ncpus=1, seed=42))

    def test_same_seed_independent_of_parallelism(self):
        self.assertEqual(
            run_proximity(hashseed=1, ncpus=1, seed=42),
            run_proximity(hashseed=9, ncpus=2, seed=42),
        )

    def test_different_seeds_change_random_background(self):
        first = run_proximity(hashseed=1, ncpus=1, seed=42)
        second = run_proximity(hashseed=1, ncpus=1, seed=777)
        self.assertEqual([r["Drug"] for r in first], [r["Drug"] for r in second])
        self.assertTrue(any(
            (a["Mean"], a["SD"]) != (b["Mean"], b["SD"])
            for a, b in zip(first, second)
        ))


if __name__ == "__main__":
    unittest.main()
