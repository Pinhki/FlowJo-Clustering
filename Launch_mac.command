#!/usr/bin/env bash
set -u

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
if [[ -d "$SCRIPT_DIR/Contents" ]]; then
  CONTENT_DIR="Contents"
elif [[ -d "$SCRIPT_DIR/Content" ]]; then
  CONTENT_DIR="Content"
else
  CONTENT_DIR="Contents"
fi
RUN_FILE="$SCRIPT_DIR/$CONTENT_DIR/run.R"
PROJECT_NAME="$(basename "$SCRIPT_DIR")"
PROJECT_PKG_ROOT="$SCRIPT_DIR/package_libraries"
PROJECT_PY_ROOT="$SCRIPT_DIR/python_envs"
EXPECTED_R_VERSION="4.4.3"
R_MAC_URL="https://cran.r-project.org/bin/macosx/"

# Keep canonical project runtime paths.
# If permissions are wrong, stop and provide an explicit one-time fix command.
export WANLAB_LIB_ROOT="$PROJECT_PKG_ROOT"
export WANLAB_PY_ENV_ROOT="$PROJECT_PY_ROOT"

# Create canonical runtime folders on first launch if they are missing.
mkdir -p "$PROJECT_PKG_ROOT" "$PROJECT_PY_ROOT" 2>/dev/null || true

if [[ ! -d "$PROJECT_PKG_ROOT" || ! -w "$PROJECT_PKG_ROOT" || ! -d "$PROJECT_PY_ROOT" || ! -w "$PROJECT_PY_ROOT" ]]; then
  echo "ERROR: runtime paths are not writable:"
  echo "  $PROJECT_PKG_ROOT"
  echo "  $PROJECT_PY_ROOT"
  echo
  echo "Run this once in Terminal, then relaunch:"
  echo "  sudo chown -R \"$(id -un)\":staff \"$PROJECT_PKG_ROOT\" \"$PROJECT_PY_ROOT\""
  echo "  chmod -R u+rwX \"$PROJECT_PKG_ROOT\" \"$PROJECT_PY_ROOT\""
  echo
  read -r -p "Press Enter to close..."
  exit 1
fi

echo "==========================================="
echo " Single-Cell RNA-Seq Pipeline Launcher"
echo "==========================================="
echo "Project: $SCRIPT_DIR"
echo

if [[ ! -f "$RUN_FILE" ]]; then
  echo "ERROR: Missing launcher script: $RUN_FILE"
  echo
  read -r -p "Press Enter to close..."
  exit 1
fi

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found."
  read -r -p "Open official R download page now? [y/N]: " open_r
  if [[ "$open_r" =~ ^[Yy]$ ]]; then
    open "$R_MAC_URL" >/dev/null 2>&1 || true
    echo "Opened: $R_MAC_URL"
  fi
  echo
  read -r -p "Press Enter to close..."
  exit 1
fi

DETECTED_R_VERSION="$(Rscript --version 2>&1 | sed -nE 's/.*version ([0-9]+\.[0-9]+\.[0-9]+).*/\1/p' | head -n1)"
if [[ "$DETECTED_R_VERSION" != "$EXPECTED_R_VERSION" ]]; then
  echo "Detected R version: ${DETECTED_R_VERSION:-unknown}"
  echo "Required R version: $EXPECTED_R_VERSION"
  read -r -p "Open official R download page now? [y/N]: " open_r
  if [[ "$open_r" =~ ^[Yy]$ ]]; then
    open "$R_MAC_URL" >/dev/null 2>&1 || true
    echo "Opened: $R_MAC_URL"
  fi
  echo
  read -r -p "Press Enter to close..."
  exit 1
fi

cd "$SCRIPT_DIR/$CONTENT_DIR" || {
  echo "ERROR: Could not enter $CONTENT_DIR/ directory."
  echo
  read -r -p "Press Enter to close..."
  exit 1
}

echo "Starting app..."
echo
Rscript run.R
RC=$?

echo
if [[ $RC -ne 0 ]]; then
  echo "App exited with error code: $RC"
  read -r -p "Press Enter to close..."
else
  echo "App exited normally."
  read -r -p "Press Enter to close..."
fi

exit $RC
