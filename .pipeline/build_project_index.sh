#!/usr/bin/env bash
set -euo pipefail
"/Users/rongzhenyu/Downloads/R_pipeline/.pipeline_tools/build_project_index.sh" "$(cd "$(dirname "$0")/.." && pwd)"
