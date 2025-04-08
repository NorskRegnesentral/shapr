#!/bin/bash

# Define filenames
PYFILE="code_py.py"
NOTEBOOK="code_py.ipynb"
EXECUTED_NOTEBOOK="code_py_executed.ipynb"
HTMLFILE="code_py.html"

# Step 0: Check for required packages and provide installation commands if missing
REQUIRED_PACKAGES=("jupytext" "nbconvert")
for PACKAGE in "${REQUIRED_PACKAGES[@]}"; do
    if ! python3 -m pip show "$PACKAGE" > /dev/null 2>&1; then
        echo "Package '$PACKAGE' is not installed. You can install it by running:"
        echo "python3 -m pip install --user $PACKAGE"
    fi
done

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
