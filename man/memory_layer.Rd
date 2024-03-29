% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/approach_vaeac_torch_modules.R
\name{memory_layer}
\alias{memory_layer}
\title{A \code{\link[torch:nn_module]{torch::nn_module()}} Representing a Memory Layer}
\usage{
memory_layer(id, output = FALSE, add = FALSE, verbose = FALSE)
}
\arguments{
\item{id}{A unique id to use as a key in the storage list.}

\item{output}{Boolean variable indicating if the memory layer is to store input in storage or extract from storage.}

\item{add}{Boolean variable indicating if the extracted value are to be added or concatenated to the input.
Only applicable when \code{output = TRUE}.}

\item{verbose}{Boolean variable indicating if we want to give printouts to the user.}
}
\description{
The layer is used to make skip-connections inside a \link[torch:nn_sequential]{torch::nn_sequential} network
or between several \link[torch:nn_sequential]{torch::nn_sequential} networks without unnecessary code complication.
}
\details{
If \code{output = FALSE}, this layer stores its input in a static list \code{storage} with the key \verb{id`` and then passes the input to the next layer. I.e., when memory layer is used in the masked encoder. If }output = TRUE\verb{, this layer takes stored tensor from the storage. I.e., when memory layer is used in the decoder. If }add = TRUE\verb{, it returns sum of the stored vector and an }input\verb{, otherwise it returns their concatenation. If the tensor with specified }id\verb{is not in storage when the layer with}output = TRUE` is called, it would cause an exception.
}
\examples{
\dontrun{
net1 <- torch::nn_sequential(
  memory_layer("#1"),
  memory_layer("#0.1"),
  torch::nn_linear(512, 256),
  torch::nn_leaky_relu(),
  # here add cannot be TRUE because the dimensions mismatch
  memory_layer("#0.1", output = TRUE, add = FALSE),
  torch::nn_linear(768, 256),
  # the dimension after the concatenation with skip-connection is 512 + 256 = 768
)
net2 <- torch::nn_equential(
  torch::nn_linear(512, 512),
  memory_layer("#1", output = TRUE, add = TRUE),
  ...
)
b <- net1(a)
d <- net2(c) # net2 must be called after net1, otherwise tensor '#1' will not be in storage.
}
}
\author{
Lars Henry Berge Olsen
}
\keyword{internal}
