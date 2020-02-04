---
# Example from https://joss.readthedocs.io/en/latest/submitting.html
title: 'shapr: An R-package for explaining machine learning models with dependence-aware Shapley values'
tags:
  - R
  - explainable AI
  - interpretable machine learning
  - shapley values
  - feature dependence
authors:
  - name: Nikolai Sellereite
    orcid: 0000-0002-4671-0337
    affiliation: 1 # (Multiple affiliations must be quoted)
  - name: Martin Jullum
    orcid: 0000-0003-3908-5155
    affiliation: 1
affiliations:
 - name: Norwegian Computing Center
   index: 1
citation_author: Sellereite and Jullum
date: 20 November 2019
year: 2019
formatted_doi: XX.XXXXX/joss.XXXXX
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---


# Summary

A common task within machine learning is to train a model to predict an unknown outcome 
(response variable) based on a set of known input variables/features.
When using such models for real life applications, it is often crucial to understand why a certain set of features lead 
to a specific prediction.
Most machine learning models are, however, complicated and hard to understand, so that they are often viewed as 
"black-boxes", that produce some output from some input.

Shapley values [@Shapley53] is a concept from cooperative game theory used to distribute fairly a joint payoff among the
cooperating players. 
@kononenko2010efficient and later @lundberg2017unified proposed to use the Shapley value framework to explain 
predictions by distributing the prediction value on the input features. 
Established methods and implementations for explaining predictions with Shapley values like Shapley 
Sampling Values [@vstrumbelj2014explaining], SHAP/Kernel SHAP [@lundberg2017unified], and to some extent 
TreeSHAP/TreeExplainer [@lundberg2018consistent; @Lundberg2020], assume that the features are independent when 
approximating the Shapley values. 
The `R`-package `shapr`, however, implements the methodology proposed by @aas2019explaining, where predictions are explained while
accounting for the dependence between the features, resulting in significantly more accurate approximations to the 
Shapley values. 



# Implementation

`shapr` implements a variant of the Kernel SHAP methodology [@lundberg2017unified] for efficiently dealing with the 
combinatorial problem related to the Shapley value formula.
The main methodological contribution of @aas2019explaining is three different methods to estimate certain conditional 
expectation quantities, referred to as the  _empirical_, _Gaussian_ and _copula_ approach. Additionaly, the user has
the option of combining the three approaches. 
The implementation supports explanation of models fitted with the following functions natively: `stats::lm` [@rCore], `stats::glm` [@rCore], 
`ranger::ranger` [@ranger], `mgcv::gam` [@mgcv] and `xgboost::xgboost`/`xgboost::xgb.train` [@xgboost]. 
Moreover, the package supports explanation of custom models by supplying two simple additional class functions.

For reference, the package also includes a benchmark implementation of the original (independence assuming) version of
Kernel SHAP [@lundberg2017unified], providing identical results to the "official" Kernel SHAP `Python` package `shap`. 
This allows the user to easily see the effect and importance of accounting for the feature dependence.

The user interface in the package has largely been adopted from the `R`-package `lime` [@limeRpackage]. 
The user first sets up the explainability framework with the `shapr` function. 
Then the output from `shapr` is provided to the `explain` function, along with the data to explain the prediction 
and the method that should be used to estimate the aforementioned conditional expectations.

The majority of the code is in plain `R` [@rCore], while the most time consuming operations are coded in `C++` 
through the `Rcpp` package [@rcppRpackage] and `RcppArmadillo` package [@eddelbuettel2014rcpparmadillo] for computational speed up. 
For RAM efficiency and computational speed up of typical bookeeping operations, we utilize the `data.table` 
package [@datatableRpackage] which does operations "by reference", i.e. without memory copies.

For a detailed description of the underlying methodology that the package implements, we refer to the 
[paper](https://arxiv.org/abs/1903.10464) [@aas2019explaining] which uses the package in examples and simulation 
studies.
To get started with the package, we recommend going through the package vignette and introductory examples
available at the package's [pkgdown site](https://norskregnesentral.github.io/shapr/). 

# Acknowledgement

This work was supported by the Norwegian Research Council grant 237718 (Big Insight).


# References
