"""Run existing DrugSigNet network functions in their own native runtime."""
import argparse
import importlib
import json
import math
from numbers import Real
from pathlib import Path
import sys
import traceback

# Fixed entry points only: no eval, shell execution, or arbitrary script paths.
FUNCTIONS = {
    "make_graphs": {"create_gt"},
    "TrustRank": {"trust_rank"},
    "Harmonic_centrality": {"harmonic_centrality"},
    "Degree_centrality": {"degree_centrality"},
    "Network_proximity": {"Network_Proximity"},
    "Diffusion_matrix_generator": {"process_ppi_network"},
    "Diffusion_run_pipelines": {"Diff_run_pipelines"},
}


def run(request, response_path):
    import pandas as pd
    from pandas.api.types import is_bool_dtype, is_numeric_dtype

    module, name = request["module"], request["function_name"]
    if name not in FUNCTIONS.get(module, set()):
        raise ValueError("Unsupported network worker entry point")
    arguments = {}
    for key, value in request["args"].items():
        if isinstance(value, dict) and value.get("kind") == "data.frame":
            value = pd.read_csv(value["path"], dtype=value["types"],
                                keep_default_na=False,
                                na_values=[request["na_value"]])
        arguments[key] = value
    result = getattr(importlib.import_module(module), name)(**arguments)
    if request["discard"]:
        return {"ok": True, "kind": "value", "value": None}
    if request["result_index"] is not None:
        result = result[request["result_index"]]
    if not isinstance(result, pd.DataFrame):
        return {"ok": True, "kind": "value", "value": result}

    result = result.copy()
    types = {}
    for column in result:
        series = result[column]
        if is_bool_dtype(series.dtype):
            types[column] = "boolean"
        elif is_numeric_dtype(series.dtype):
            types[column] = "number"
            # Keep floating NaN distinct from nullable missing values in R.
            result[column] = pd.Series([
                "NaN" if isinstance(value, Real) and math.isnan(value) else value
                for value in series
            ], index=series.index, dtype=object)
        elif any(isinstance(value, (set, frozenset, list, tuple)) for value in series):
            types[column] = "list"
            # Unordered Python sets become ordinary, serializable R vectors.
            result[column] = series.map(lambda value: json.dumps(
                sorted(value) if isinstance(value, (set, frozenset)) else list(value)
            ) if isinstance(value, (set, frozenset, list, tuple)) else value)
        else:
            types[column] = "string"
    table = response_path.with_suffix(".csv")
    result.to_csv(table, index=False, na_rep=request["na_value"], float_format="%.17g")
    return {"ok": True, "kind": "data.frame", "path": str(table), "types": types}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--request", required=True)
    parser.add_argument("--response", required=True)
    args = parser.parse_args()
    response = Path(args.response)
    try:
        request = json.loads(Path(args.request).read_text(encoding="utf-8"))
        result = run(request, response)
        response.write_text(json.dumps(result), encoding="utf-8")
    except Exception as error:
        response.write_text(json.dumps({"ok": False, "error": str(error)}), encoding="utf-8")
        traceback.print_exc()
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
