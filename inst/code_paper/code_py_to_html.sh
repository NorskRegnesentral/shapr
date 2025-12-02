#!/bin/bash

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

# Step 0: Check for required packages and provide installation commands if missing
REQUIRED_PACKAGES=("jupyter" "jupytext" "nbconvert")
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
jupyter nbconvert --to html "$EXECUTED_NOTEBOOK" --output "$HTMLFILE" \
  --TagRemovePreprocessor.enabled=True \
  --TagRemovePreprocessor.remove_input_tags="['hide_input']" 2>/dev/null

# Step 4: Cleanup intermediate files
echo "Cleaning up..."
rm -f "$NOTEBOOK" "$EXECUTED_NOTEBOOK"

echo "✅ Done! Output written to $HTMLFILE"
