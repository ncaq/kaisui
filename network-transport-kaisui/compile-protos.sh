#!/usr/bin/env bash
set -euo pipefail

# Compile ractor_cluster proto files
compile-proto-file \
  --includeDir proto/ractor_cluster \
  --proto meta.proto \
  --out src/Proto/RactorCluster/

compile-proto-file \
  --includeDir proto/ractor_cluster \
  --proto auth.proto \
  --out src/Proto/RactorCluster/

compile-proto-file \
  --includeDir proto/ractor_cluster \
  --proto node.proto \
  --out src/Proto/RactorCluster/

compile-proto-file \
  --includeDir proto/ractor_cluster \
  --proto control.proto \
  --out src/Proto/RactorCluster/

echo "Proto files compiled successfully"
