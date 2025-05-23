
# How to reproduce the code and figure examples in the paper 
# shapr: Explaining Machine Learning Models with Conditional Shapley Values in R and Python

*The following instructions assumes a Linux type environment.*
*Modifications may be required to run them on other operating systems.*

*The code has been tested on Ubuntu 20.04.6 LTS with R 4.4.1 and Python 3.12.7 installed.*


## R
To reproduce the R examples and figure, make sure you have `R>=3.5` installed along with the `shapr` package, in addition to the following packages from CRAN: `xgboost`, `data.table`, `future`, `progressr` and `patchwork`.

Then, from the command line, run 
```
Rscript -e "knitr::spin('code_R.R')"
```
This will generate the file `code_R.html` containing the code from `code_R.R` accompanied with it's output.

**Note 1:** The html file displays the code and output of the code displayed in the paper. Additional code used to mildly customize and save the figures is provided in the `code_R.R` file and executed by `knitr::spin()`, but not shown in the html-file. 

**Note 2:** The file `R_prep_data_and_model.R` generate the data and models files used by the below script. This is already done and the files are included in the `data_and_models` folder to ensure reproducibility across different environments.

## Python

To reproduce the Python example, first make sure you have `pip` installed in addition to `R` and the `shapr` R-package.
Then, install the `shaprpy` Python package by navigating to the `python` folder (`../python` from package root, (`../../python`) from the folder of this file), and run the following from the command line 

```
pip install -e .
```

We have created a simple bash script executing the Python code in a manner similar to how the `knitr::spin()` function operates for the the `R` code.
The bash script requires the `jupytext` `nbconvert` and `session_info` libraries to run.
They can installed by with `pip` as follows:

```
pip install jupytext nbconvert session_info
```

After successfull installation, from the command line, run 
```
bash code_py_to_html.sh
```
This will generate the file `code_py.html` containing the code from `code_py.py` accompanied with it's output and basic session information.
