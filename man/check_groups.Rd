% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features.R
\name{check_groups}
\alias{check_groups}
\title{Check that the group parameter has the right form and content}
\usage{
check_groups(feature_labels, group)
}
\arguments{
\item{feature_labels}{Vector of characters. Contains the feature labels used by the model}

\item{group}{List. If \code{NULL} regular feature wise Shapley values are computed.
If provided, group wise Shapley values are computed. \code{group} then has length equal to
the number of groups. The list element contains character vectors with the features included
in each of the different groups.}
}
\value{
Error or NULL
}
\description{
Check that the group parameter has the right form and content
}
\keyword{internal}
