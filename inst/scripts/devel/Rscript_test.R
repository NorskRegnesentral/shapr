

args <- commandArgs(trailingOnly = TRUE)

p <- as.numeric(args[1])
n_train <- as.numeric(args[2])
n_test <- as.numeric(args[3])
n_batches <- as.numeric(args[4])
n_cores <- as.numeric(args[5])
approach <- args[6]

print(.libPaths())

print(p)
print(n_train)
print(n_test)
print(n_batches)
print(n_cores)
print(approach)
