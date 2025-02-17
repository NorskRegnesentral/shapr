% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shapley_setup.R
\name{sample_coalition_table}
\alias{sample_coalition_table}
\title{Get table with sampled coalitions}
\usage{
sample_coalition_table(
  m,
  n_coalitions = 200,
  weight_zero_m = 10^6,
  paired_shap_sampling = TRUE,
  prev_coal_samples = NULL,
  prev_coal_samples_n_unique = NULL,
  kernelSHAP_reweighting,
  n_samps_scale = 10,
  dt_valid_causal_coalitions = NULL
)
}
\arguments{
\item{m}{Positive integer.
Total number of features/groups.}

\item{n_coalitions}{Positive integer.
Note that if \code{exact = TRUE}, \code{n_coalitions} is ignored.}

\item{weight_zero_m}{Numeric.
The value to use as a replacement for infinite coalition weights when doing numerical operations.}

\item{paired_shap_sampling}{Logical.
Whether to do paired sampling of coalitions.}

\item{prev_coal_samples}{Character vector.
A vector of previously sampled coalitions as characters.
Each string contains a coalition and the feature indices in the coalition is separated by a space.
For example, "1 5 8" is a coalition with features 1, 5, and 8.}

\item{prev_coal_samples_n_unique}{Positive integer.
The number of unique coalitions in \code{prev_coal_samples}.
This is a separate argument to avoid recomputing the number unnecessarily.}

\item{n_samps_scale}{Positive integer.
Integer that scales the number of coalitions \code{n_coalitions} to sample as sampling is cheap,
while checking for \code{n_coalitions} unique coalitions is expensive, thus we over sample the
number of coalitions by a factor of \code{n_samps_scale} and determine when we have \code{n_coalitions} unique
coalitions and only use the coalitions up to this point and throw away the remaining coalitions.}

\item{dt_valid_causal_coalitions}{data.table. Only applicable for asymmetric Shapley
values explanations, and is \code{NULL} for symmetric Shapley values.
The data.table contains information about the coalitions that respects the causal ordering.}
}
\description{
Get table with sampled coalitions
}
\keyword{internal}
