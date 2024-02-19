# Library ---------------------------------------------------------------------------------------------------------
library("torch")
library("rbenchmark")

# Functions -------------------------------------------------------------------------------------------------------
vaeac_dataset_item <- torch::dataset(
  name = "vaeac_dataset", # field name The name of the `torch::dataset`.

  # description Create a new vaeac_dataset object.
  # param X A torch_tensor containing the data
  # param one_hot_max_sizes A torch tensor of dimension p containing the one hot sizes of the p features.
  # The sizes for the continuous features can either be '0' or '1'.
  initialize = function(X, one_hot_max_sizes) {
    # Save the number of observations and features in X, the one hot dummy feature sizes and the dataset
    self$N <- nrow(X)
    self$p <- ncol(X)
    self$one_hot_max_sizes <- one_hot_max_sizes
    self$X <- X
  },
  .getitem = function(index) self$X[index, ],  # Get a single data observation based on the provided index
  .length = function() nrow(self$X) # Get the number of observations in the dataset
)

vaeac_dataset_batch <- torch::dataset(
  name = "vaeac_dataset", # field name The name of the `torch::dataset`.

  # description Create a new vaeac_dataset object.
  # param X A torch_tensor containing the data
  # param one_hot_max_sizes A torch tensor of dimension p containing the one hot sizes of the p features.
  # The sizes for the continuous features can either be '0' or '1'.
  initialize = function(X, one_hot_max_sizes) {
    # Save the number of observations and features in X, the one hot dummy feature sizes and the dataset
    self$N <- nrow(X)
    self$p <- ncol(X)
    self$one_hot_max_sizes <- one_hot_max_sizes
    self$X <- X
  },
  .getbatch = function(index) self$X[index, , drop = FALSE],  # Get a batch of data based on the provided indices
  .length = function() nrow(self$X) # Get the number of observations in the dataset
)

# Parameters ------------------------------------------------------------------------------------------------------
p <- 10
N <- 5000
batch_size <- 64
one_hot_max_sizes <- rep(1, p)

# Create data -----------------------------------------------------------------------------------------------------
data <- matrix(rnorm(p * N), ncol = p)

# Comparison ------------------------------------------------------------------------------------------------------
# Create the datasets
vaeac_ds_item <- vaeac_dataset_item(torch_tensor(data = data, dtype = torch_float()), one_hot_max_sizes)
vaeac_ds_batch <- vaeac_dataset_batch(torch_tensor(data = data, dtype = torch_float()), one_hot_max_sizes)

# Create the dataloaders
vaeac_dl_item <- torch::dataloader(vaeac_ds_item, batch_size = batch_size, shuffle = TRUE, drop_last = FALSE)
vaeac_dl_batch <- torch::dataloader(vaeac_ds_batch, batch_size = batch_size, shuffle = TRUE, drop_last = FALSE)

# Same length
vaeac_dl_item$.length()
vaeac_dl_batch$.length()

## Compare values --------------------------------------------------------------------------------------------------
# Check that .getitem and .getbatch yields the same results
torch_manual_seed(123)
vaeac_iterator_item <- vaeac_dl_item$.iter()
bi1 <- vaeac_iterator_item$.next() # batch1
bi2 <- vaeac_iterator_item$.next() # batch2

torch_manual_seed(123)
vaeac_iterator_batch <- vaeac_dl_batch$.iter()
bb1 <- vaeac_iterator_batch$.next() # batch1
bb2 <- vaeac_iterator_batch$.next() # batch2

all.equal(as.matrix(bi1), as.matrix(bb1))
all.equal(as.matrix(bi2), as.matrix(bb2))

## Compare time ----------------------------------------------------------------------------------------------------
# Loop over all the data and look at time
time_item <- system.time(coro::loop(for (b in vaeac_dl_item) print(b)))
time_batch <- system.time(coro::loop(for (b in vaeac_dl_batch) print(b)))
rbind(time_item, time_batch)

# Do it more systematic
benchmark(
  item = coro::loop(for (b in vaeac_dl_item) {}),
  batch = coro::loop(for (b in vaeac_dl_batch) {})
)

# We see that batch is much faster (quite obvious in hindsight)
# p <- 5, N <- 5000, batch_size <- 64
#    test replications elapsed relative user.self sys.self user.child sys.child
# 2 batch          100   1.684    1.000     1.665    0.013          0         0
# 1  item          100  31.631   18.783    31.382    0.163          0         0

# p <- 5, N <- 5000, batch_size <- 128
#    test replications elapsed relative user.self sys.self user.child sys.child
# 2 batch          100   1.163    1.000     1.117    0.023      0.000     0.000
# 1  item          100  33.441   28.754    32.501    0.459      0.021     0.037

# p <- 10, N <- 10000, batch_size <- 64
#    test replications elapsed relative user.self sys.self user.child sys.child
# 2 batch          100   3.732    1.000     3.551    0.064          0         0
# 1  item          100  64.719   17.342    63.870    0.383          0         0

# p <- 10, N <- 10000, batch_size <- 128
#    test replications elapsed relative user.self sys.self user.child sys.child
# 2 batch          100   1.681    1.000     1.659    0.020       0.00     0.000
# 1  item          100  67.040   39.881    65.543    0.721       0.02     0.031

# p <- 15, N <- 1000, batch_size <- 64
#    test replications elapsed relative user.self sys.self user.child sys.child
# 2 batch          100   0.528    1.000     0.525    0.003          0         0
# 1  item          100   6.254   11.845     6.155    0.046          0         0

# p <- 15, N <- 1000, batch_size <- 128
#    test replications elapsed relative user.self sys.self user.child sys.child
# 2 batch          100   0.474    1.000     0.462    0.008          0         0
# 1  item          100   6.692   14.118     6.499    0.091          0         0
