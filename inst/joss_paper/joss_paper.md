---
# Example from https://joss.readthedocs.io/en/latest/submitting.html
title: 'shapr: An R package for explaining machine learning models with dependence-aware Shapley values'
tags:
  - R
  - explainable AI
  - interpretable machine learning
  - shapley values
  - feature dependence
authors:
  - name: Nikolai Sellereite
    orcid: 0000-0000-0000-0000
    affiliation: 1 # (Multiple affiliations must be quoted)
  - name: Martin Jullum
    orcid: 0000-0003-3908-5155
    affiliation: 1
affiliations:
 - name: Norwegian Computing Center
   index: 1
#citation_author: Sellereite and Jullum
date: 20 November 2018
#year: 2018
#formatted_doi: XX.XXXXX/joss.XXXXX
bibliography: paper.bib
output: rticles::joss_article
#csl: apa.csl
journal: JOSS
---

<!-- What should my paper contain? -->
<!-- Important -->

<!-- Begin your paper with a summary of the high-level functionality of your software for a non-specialist reader. Avoid jargon in this section. -->

<!-- JOSS welcomes submissions from broadly diverse research areas. For this reason, we require that authors include in the paper some sentences that explain the software functionality and domain of use to a non-specialist reader. We also require that authors explain the research applications of the software. The paper should be between 250-1000 words. -->

<!-- Your paper should include: -->

<!-- A list of the authors of the software and their affiliations, using the correct format (see the example below). -->
<!-- A summary describing the high-level functionality and purpose of the software for a diverse, non-specialist audience. -->
<!-- A clear Statement of Need that illustrates the research purpose of the software. -->
<!-- A list of key references, including to other software addressing related needs. -->
<!-- Mention (if applicable) of any past or ongoing research projects using the software and recent scholarly publications enabled by it. -->
<!-- Acknowledgement of any financial support. -->
<!-- As this short list shows, JOSS papers are only expected to contain a limited set of metadata (see example below), a Statement of Need, Summary, Acknowledgements, and References sections. You can look at an example accepted paper. Given this format, a “full length” paper is not permitted, and software documentation such as API (Application Programming Interface) functionality should not be in the paper and instead should be outlined in the software documentation. 

USE: devtools::install_github("benmarwick/wordcountaddin", type = "source", dependencies = TRUE)

for word counting in the Rmarkdwon document (250-1000 words)

-->


# Summary

A common task within machine learning is to train a model which is able to predict an unknown outcome (response variable) based on a set of known input variables/features.
When using such models for real life applications, it is often crucial to understand why a certain set of features lead to exactly a specific prediction.
Most machine learning models are however so complicated and hard to understand that they are often viewed as "black-boxes" producing output when provided some input.

Shapley values (@Shapley53) is a concept from cooperative game theory used to fairly distribute a joint payoff among the cooperating players. @kononenko2010efficient and later @lundberg2017unified proposed to use the Shapley value framework to explain predictions by distributing the prediction value on the input features. Unfortunately, established methods and implementations for explaining predictions with Shapley values like Shapley Sampling Values (@vstrumbelj2014explaining), SHAP/Kernel SHAP (@lundberg2017unified), and to some extent TreeSHAP (@lundberg2018consistent), assume that the features are independent when approximating the Shapley values for prediction explanation. This R-package implements methodology proposed by @aas2019explaining to explain predictions by accounting for the dependence between the features, resulting in significantly more accurate approximations to the Shapley values. 



# Implementation

The package relies on the Kernel SHAP (@lundberg2017unified) methodology for efficiently dealing with combinatorial problems related to Shapley values. 

Different methods (Gauss, copula nad empirical), user flexibiloty to choose method tailored for specific need, but with good default values.

Which models handled natively -- support from custom models

Style adopted from the lime R package

Rcpp for speed up of some functions.

Faster than the KernelSHAP implemented in SHAP Python package.



# Acknowledgement

This work was supported by the Norwegian Research Council grant 237718 (Big Insight).


# Notes (do be deleted)

Mention and refer to the SHAP Python package

Manually transform this document to .md file when we are done to allow for automatic online compilation


# References



<!-- @vstrumbelj2014explaining  -->

<!-- @lundberg2017unified -->

<!-- the only prediction explanation framework with a solid theoretical foundation (@lundberg2017unified).  -->

<!-- Unless the true distribution of the features are known, and there are less than say 10-15 features, these Shapley values needs to be estimated/approximated.  -->
<!-- Previous known methods  -->

<!-- Popular methods like Shapley Sampling Values (@vstrumbelj2014explaining), SHAP/Kernel SHAP (@lundberg2017unified), and to some extent TreeSHAP (@lundberg2018consistent), assume that the features are independent when approximating the Shapley values for prediction explanation. This may lead to very inaccurate Shapley values, and consequently wrong interpretations of the predictions. @aas2019explaining extends and improves the Kernel SHAP method of @lundberg2017unified to account for the dependence between the features, resulting in significantly more accurate approximations to the Shapley values.  -->
<!-- [See the paper for details](https://arxiv.org/abs/1903.10464). -->


<!-- # The Kernel SHAP Method -->

<!-- Assume a predictive model $f(\boldsymbol{x})$ for a response value $y$ with features  -->
<!-- $\boldsymbol{x}\in \mathbb{R}^M$, trained on a training set, and that we want to explain the  -->
<!-- predictions for new sets of features. This may be done using ideas from cooperative game theory,  -->
<!-- letting a single prediction take the place of the game being played and the features the place of  -->
<!-- the players. Letting $N$ denote the set of all $M$ players, and $S \subseteq N$ be a subset  -->
<!-- of $|S|$ players, the "contribution" function $v(S)$ describes the total expected sum of payoffs  -->
<!-- the members of $S$ can obtain by cooperation. The Shapley value (@Shapley53) is one way to  -->
<!-- distribute the total gains to the players, assuming that they all collaborate. The amount that  -->
<!-- player $i$ gets is then -->

<!-- $$\phi_i(v) = \phi_i = \sum_{S \subseteq N \setminus\{i\}} \frac{|S| ! (M-| S| - 1)!}{M!}(v(S\cup \{i\})-v(S)),$$ -->


<!-- that is, a weighted mean over all subsets $S$ of players not containing player $i$.  -->
<!-- @lundberg2017unified define the contribution function for a certain subset $S$ of these features  -->
<!-- $\boldsymbol{x}_S$ as $v(S) = \mbox{E}[f(\boldsymbol{x})|\boldsymbol{x}_S]$, the expected output  -->
<!-- of the predictive model conditional on the feature values of the subset. @lundberg2017unified names -->
<!-- this type of Shapley values SHAP (SHapley Additive exPlanation) values. Since the conditional  -->
<!-- expectations can be written as  -->

<!-- $$E[f(\boldsymbol{x})|\boldsymbol{x}_s=\boldsymbol{x}_S^*] = E[f(\boldsymbol{x}_{\bar{S}},\boldsymbol{x}_S)|\boldsymbol{x}_S=\boldsymbol{x}_S^*] = \int f(\boldsymbol{x}_{\bar{S}},\boldsymbol{x}_S^*)\,p(\boldsymbol{x}_{\bar{S}}|\boldsymbol{x}_S=\boldsymbol{x}_S^*)d\boldsymbol{x}_{\bar{S}},$$ -->


<!-- the conditional distributions $p(\boldsymbol{x}_{\bar{S}}|\boldsymbol{x}_S=\boldsymbol{x}_S^*)$ are  -->
<!-- needed to compute the contributions. The Kernel SHAP method of @lundberg2017unified assumes feature  -->
<!-- independence, so that $p(\boldsymbol{x}_{\bar{S}}|\boldsymbol{x}_S=\boldsymbol{x}_S^*)=p(\boldsymbol{x}_{\bar{S}})$.  -->
<!-- If samples $\boldsymbol{x}_{\bar{S}}^{k}, k=1,\ldots,K$, from $p(\boldsymbol{x}_{\bar{S}}|\boldsymbol{x}_S=\boldsymbol{x}_S^*)$  -->
<!-- are available, the conditional expectation in above can be approximated by  -->

<!-- $$  v_{\text{KerSHAP}}(S) = \frac{1}{K}\sum_{k=1}^K f(\boldsymbol{x}_{\bar{S}}^{k},\boldsymbol{x}_S^*).$$ -->


<!-- In Kernel SHAP, $\boldsymbol{x}_{\bar{S}}^{k}, k=1,\ldots,K$ are sampled from the $\bar{S}$-part of  -->
<!-- the training data, *independently* of $\boldsymbol{x}_{S}$. This is motivated by using the  -->
<!-- training set as the empirical distribution of $\boldsymbol{x}_{\bar{S}}$, and assuming that  -->
<!-- $\boldsymbol{x}_{\bar{S}}$ is independent of $\boldsymbol{x}_S=\boldsymbol{x}_S^*$. -->
<!-- Due to the independence assumption, if the features in a given model are highly dependent, the  -->
<!-- Kernel SHAP method may give a completely wrong answer. This can be avoided by estimating the  -->
<!-- conditional distribution $p(\boldsymbol{x}_{\bar{S}}|\boldsymbol{x}_S=\boldsymbol{x}_S^*)$ directly  -->
<!-- and generating samples from this distribution. With this small change, the contributions and  -->
<!-- Shapley values may then be approximated as in the ordinary Kernel SHAP framework. @aas2019explaining  -->
<!-- propose three different approaches for estimating the conditional probabilities. The methods may  -->
<!-- also be combined, such that e.g. one method is used when conditioning on a small number of features,  -->
<!-- while another method is used otherwise. -->


<!-- ## Multivariate Gaussian Distribution Approach -->

<!-- The first approach arises from the assumption that the feature vector $\boldsymbol{x}$ stems from a  -->
<!-- multivariate Gaussian distribution with some mean vector $\boldsymbol{\mu}$ and covariance matrix  -->
<!-- $\boldsymbol{\Sigma}$. Under this assumption, the conditional distribution  -->
<!-- $p(\boldsymbol{x}_{\bar{\mathcal{S}}} |\boldsymbol{x}_{\mathcal{S}}=\boldsymbol{x}_{\mathcal{S}}^*)$  -->
<!-- is also multivariate Gaussian  $\text{N}_{|\bar{\mathcal{S}}|}(\boldsymbol{\mu}_{\bar{\mathcal{S}}|\mathcal{S}},\boldsymbol{\Sigma}_{\bar{\mathcal{S}}|\mathcal{S}})$,  -->
<!-- with analytical expressions for the conditional mean vector $\boldsymbol{\mu}_{\bar{\mathcal{S}}|\mathcal{S}}$  -->
<!-- and covariance matrix $\boldsymbol{\Sigma}_{\bar{\mathcal{S}}|\mathcal{S}}$, see @aas2019explaining for details. -->
<!-- Hence, instead of sampling from the marginal empirical distribution of $\boldsymbol{x}_{\bar{\mathcal{S}}}$  -->
<!-- approximated by the training data, we can sample from the Gaussian conditional distribution, which is fitted  -->
<!-- using the training data. Using the resulting samples  -->
<!-- $\boldsymbol{x}_{\bar{\mathcal{S}}}^k, k=1,\ldots,K$, the conditional expectations be approximated  -->
<!-- as in the Kernel SHAP.  -->


<!-- ## Gaussian Copula Approach -->

<!-- If the features are far from multivariate Gaussian, an alternative approach is to instead represent  -->
<!-- the marginals by their empirical distributions, and model the dependence structure by a Gaussian  -->
<!-- copula. Assuming a Gaussian copula, we may convert the marginals of the training data to Gaussian  -->
<!-- features using their empirical distributions, and then fit a multivariate Gaussian distribution to these.  -->

<!-- To produce samples from the conditional distribution $p(\boldsymbol{x}_{\bar{\mathcal{S}}} |\boldsymbol{x}_{\mathcal{S}}=\boldsymbol{x}_{\mathcal{S}}^*)$,  -->
<!-- we convert the marginals of $\boldsymbol{x}_{\mathcal{S}}$ to Gaussians, sample from the conditional -->
<!-- Gaussian distribution as above, and convert the marginals of the samples back to the original  -->
<!-- distribution. Those samples are then used to approximate the sample from the resulting multivariate  -->
<!-- Gaussian conditional distribution. While other copulas may be used, the Gaussian copula has the  -->
<!-- benefit that we may use the analytical expressions for the conditionals  -->
<!-- $\boldsymbol{\mu}_{\bar{\mathcal{S}}|\mathcal{S}}$ and $\boldsymbol{\Sigma}_{\bar{\mathcal{S}}|\mathcal{S}}$.  -->
<!-- Finally, we may convert the marginals back to their original distribution, and use the resulting  -->
<!-- samples to approximate the conditional expectations as in the Kernel SHAP.   -->

<!-- ## Empirical Conditional Distribution Approach -->

<!-- If both the dependence structure and the marginal distributions of $\boldsymbol{x}$ are very far  -->
<!-- from the Gaussian, neither of the two aforementioned methods will work very well. Few methods  -->
<!-- exists for the non-parametric estimation of conditional densities, and the classic kernel  -->
<!-- estimator (@rosenblatt1956) for non-parametric density estimation suffers greatly from the  -->
<!-- curse of dimensionality and does not provide a way to generate samples from the estimated  -->
<!-- distribution. For such situations, @aas2019explaining propose an empirical conditional approach  -->
<!-- to sample approximately from $p(\boldsymbol{x}_{\bar{\mathcal{S}}}|\boldsymbol{x}_{\mathcal{S}}^*)$.  -->
<!-- The idea is to compute weights $w_{\mathcal{S}}(\boldsymbol{x}^*,\boldsymbol{x}^i),\ i=1,...,n_{\text{train}}$  -->
<!-- for all training instances based on their Mahalanobis distances (in the $S$ subset only) to the  -->
<!-- instance $\boldsymbol{x}^*$ to be explained. Instead of sampling from this weighted (conditional)  -->
<!-- empirical distribution, @aas2019explaining suggests a more efficient variant, using only the $K$  -->
<!-- instances with the largest weights: -->

<!-- $$v_{\text{condKerSHAP}}(\mathcal{S}) = \frac{\sum_{k=1}^K w_{\mathcal{S}}(\boldsymbol{x}^*,\boldsymbol{x}^{[k]}) f(\boldsymbol{x}_{\bar{\mathcal{S}}}^{[k]},\boldsymbol{x}_{\mathcal{S}}^*)}{\sum_{k=1}^K w_{\mathcal{S}}(\boldsymbol{x}^*,\boldsymbol{x}^{[k]})},$$  -->

<!-- The number of samples $K$ to be used in the approximate prediction can for instance be chosen such  -->
<!-- that the $K$ largest weights accounts for a fraction $\eta$, for example $0.9$, of the total weight.  -->
<!-- If $K$ exceeds a certain limit, for instance $5,000$, it might be set to that limit. A bandwidth -->
<!-- parameter $\sigma$ used to scale the weights, must also be specified. This choice may be viewed as  -->
<!-- a bias-variance trade-off. A small $\sigma$ puts most of the weight to a few of the closest  -->
<!-- training observations and thereby gives low bias, but high variance. When $\sigma \rightarrow \infty$,  -->
<!-- this method converges to the original Kernel SHAP assuming feature independence. Typically, when  -->
<!-- the features are highly dependent, a small $\sigma$ is typically needed such that the bias does  -->
<!-- not dominate. @aas2019explaining show that a proper criterion for selecting $\sigma$ is a  -->
<!-- small-sample-size corrected version of the AIC known as AICc. As calculation of it is  -->
<!-- computationally intensive, an approximate version of the selection criterion is also suggested.  -->
<!-- Details on this is found in @aas2019explaining. -->

