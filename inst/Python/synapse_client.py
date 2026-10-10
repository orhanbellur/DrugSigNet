"""Run Synapse downloads out-of-process to isolate conda OpenSSL from R."""

import json
import os
import sys

import synapseclient


entity_id, download_text, download_location = sys.argv[1:4]
token = os.environ.get("SYNAPSE_AUTH_TOKEN", "")
if not token:
    raise RuntimeError("SYNAPSE_AUTH_TOKEN is empty")

syn = synapseclient.Synapse()
syn.login(authToken=token, silent=True)
kwargs = {"downloadFile": download_text == "true"}
if download_location:
    kwargs["downloadLocation"] = download_location
    kwargs["ifcollision"] = "overwrite.local"
entity = syn.get(entity_id, **kwargs)
properties = entity.properties
print(json.dumps({
    "properties": {
        "id": properties.get("id"),
        "name": properties.get("name"),
        "versionNumber": properties.get("versionNumber"),
    },
    "path": getattr(entity, "path", None),
}))
