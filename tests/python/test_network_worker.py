"""Protocol regression tests; run with python -m unittest discover -s tests/python."""
import importlib.util
import json
import math
from pathlib import Path
import tempfile
from types import SimpleNamespace
import unittest
from unittest.mock import patch

import pandas as pd

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location("network_worker", ROOT / "inst/Python/network_worker.py")
worker = importlib.util.module_from_spec(spec)
spec.loader.exec_module(worker)


class NetworkWorkerTests(unittest.TestCase):
    def request(self):
        return {"module": "TrustRank", "function_name": "trust_rank", "args": {},
                "result_index": 1, "discard": False, "na_value": "__DS_MISSING__"}

    def test_typed_table_keeps_identifiers_nonfinite_values_and_targets(self):
        frame = pd.DataFrame({"Drug": ["0001", "NA", "long-\u03b2"],
                              "Score": [float("inf"), float("nan"), 1.0000000000000002],
                              "Targets": [frozenset(["002", "NA"]), frozenset(), frozenset(["G1"])]})
        with tempfile.TemporaryDirectory() as directory:
            module = SimpleNamespace(trust_rank=lambda: (None, frame))
            original_import = worker.importlib.import_module
            def load(name, *args, **kwargs):
                return module if name == "TrustRank" else original_import(name, *args, **kwargs)
            with patch.object(worker.importlib, "import_module", side_effect=load):
                response = worker.run(self.request(), Path(directory) / "response.json")
            restored = pd.read_csv(response["path"], dtype={"Drug": str}, keep_default_na=False)
            self.assertEqual(restored.Drug.tolist(), frame.Drug.tolist())
            self.assertTrue(math.isinf(float(restored.Score.iloc[0])))
            self.assertTrue(math.isnan(float(restored.Score.iloc[1])))
            self.assertEqual(float(restored.Score.iloc[2]), frame.Score.iloc[2])
            self.assertEqual(json.loads(restored.Targets.iloc[0]), ["002", "NA"])
            self.assertEqual(json.loads(restored.Targets.iloc[1]), [])
            self.assertEqual(response["types"]["Score"], "number")
            self.assertEqual(response["types"]["Targets"], "list")

    def test_unknown_entry_point_is_rejected_before_import(self):
        request = self.request()
        request.update(module="os", function_name="not_allowed")
        with patch.object(worker.importlib, "import_module") as importer:
            with self.assertRaisesRegex(ValueError, "Unsupported"):
                worker.run(request, Path("unused.json"))
            importer.assert_not_called()

    def test_worker_discards_native_graph_objects(self):
        request = self.request()
        request.update(module="make_graphs", function_name="create_gt", discard=True)
        with patch.object(worker.importlib, "import_module", return_value=SimpleNamespace(create_gt=object)):
            response = worker.run(request, Path("unused.json"))
        self.assertTrue(response["ok"])
        self.assertIsNone(response["value"])


if __name__ == "__main__":
    unittest.main()
