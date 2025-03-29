#!/bin/bash

# Define filenames
PYFILE="code_py.py"
NOTEBOOK="code_py.ipynb"
EXECUTED_NOTEBOOK="code_py_executed.ipynb"
HTMLFILE="code_py.html"

# Step 1: Convert .py to .ipynb
echo "Converting $PYFILE to notebook..."
jupytext "$PYFILE" --to notebook -o "$NOTEBOOK"

# Step 2: Execute the notebook
echo "Executing notebook..."
jupyter nbconvert --to notebook --execute "$NOTEBOOK" --output "$EXECUTED_NOTEBOOK"

# Step 3: Convert executed notebook to HTML
echo "Exporting to HTML..."
jupyter nbconvert --to html "$EXECUTED_NOTEBOOK" --output "$HTMLFILE"

# Step 4: Cleanup intermediate files
echo "Cleaning up..."
rm -f "$NOTEBOOK" "$EXECUTED_NOTEBOOK"

echo "✅ Done! Output written to $HTMLFILE"
