#!/bin/bash

set -euo pipefail

## This bash script requires jupytext and nbconvert to be installed. They can be installed using:
# pip install jupyter jupytext nbconvert

## The code_py.py script ends by listing session information, provided by the session_info package, which needs
## to be installed alongside shaprpy. session_info can be installed by running:
# pip install session_info

## Run the below command in the terminal from this script's folder to generate the code_py.html from code_py.py:
# bash code_py_to_html.sh

# Define filenames
PYFILE="code_py.py"
NOTEBOOK="code_py.ipynb"
EXECUTED_NOTEBOOK="code_py_executed.ipynb"
HTMLFILE="code_py.html"

# Step 0: Check for required commands
if ! command -v jupytext > /dev/null 2>&1 ||
   ! command -v jupyter > /dev/null 2>&1 ||
   ! jupyter nbconvert --version > /dev/null 2>&1; then
  echo "Required Jupyter tools are unavailable. Install them with:"
  echo "python3 -m pip install --user jupyter jupytext nbconvert ipykernel"
  exit 1
fi

# Step 1: Convert .py to .ipynb
echo "Converting $PYFILE to notebook..."
jupytext "$PYFILE" --to notebook -o "$NOTEBOOK"

# Step 2: Execute the notebook
echo "Executing notebook..."
jupyter nbconvert --to notebook --execute "$NOTEBOOK" --output "$EXECUTED_NOTEBOOK" \
  --KernelManager.transport=ipc

# Step 3: Convert executed notebook to HTML
echo "Exporting to HTML..."
jupyter nbconvert --to html "$EXECUTED_NOTEBOOK" --output "$HTMLFILE" \
  --TagRemovePreprocessor.enabled=True \
  --TagRemovePreprocessor.remove_input_tags="['hide_input']" 2>/dev/null

# Step 4: Cleanup intermediate files
echo "Cleaning up..."
rm -f "$NOTEBOOK" "$EXECUTED_NOTEBOOK"

echo "✅ Done! Output written to $HTMLFILE"
