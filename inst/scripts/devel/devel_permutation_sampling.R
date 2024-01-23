

unrank_permutation <- function(rank, n) {
  elements <- 1:n
  permutation <- integer(n)

  for(i in 1:n){
    factorial <- factorial(n - i)
    index <- rank %/% factorial
    rank <- rank %% factorial
    permutation[i] <- elements[index+1]
    elements <- elements[-(index+1)]
  }

  return(permutation)
}

unrank_permutations <- function(ranks, n) {
  # Precompute factorials
  factorials <- sapply(0:(n-1), factorial)

  # Initialize the list of permutations
  permutations <- vector("list", length(ranks))

  for (r in seq_along(ranks)) {
    rank <- ranks[r]
    elements <- 1:n
    permutation <- integer(n)

    for(i in seq_len(n)){
      factorial <- factorials[n - i + 1]
      index <- rank %/% factorial
      rank <- rank %% factorial
      permutation[i] <- elements[index + 1]
      elements <- elements[-(index + 1)]
    }

    permutations[[r]] <- permutation
  }

  return(permutations)
}

unrank_permutations_mat <- function(ranks, n) {
  # Precompute factorials
  factorials <- sapply(0:(n-1), factorial)

  # Initialize the matrix of permutations
  permutations <- matrix(nrow = length(ranks), ncol = n)

  for (r in seq_along(ranks)) {
    rank <- ranks[r]
    elements <- 1:n
    permutation <- integer(n)

    for(i in seq_len(n)){
      factorial <- factorials[n - i + 1]
      index <- rank %/% factorial
      rank <- rank %% factorial
      permutation[i] <- elements[index + 1]
      elements <- elements[-(index + 1)]
    }

    permutations[r, ] <- permutation
  }

  return(permutations)
}

aa=unrank_permutations_mat(seq_len(factorial(n))-1,n)

n <- 8
tot_no_permutations <- factorial(n)

samples <- factorial(n)#5*10^5

ranks = sample.int(tot_no_permutations, samples, replace = FALSE)

perms <- lapply(ranks, unrank_permutation, n = n)

perms <- unrank_permutations(1:24,n) # This is the fastest

#### My coding here ####

#### THIS SEEMS TO WORK

n <- 4
tot_no_permutations <- factorial(n)

samples <- 10#factorial(n)#5*10^5

ranks = sample.int(tot_no_permutations, samples, replace = FALSE)-1

perms <- unrank_permutations_mat(ranks,n) # This is the fastest

rev_perms <- perms[,seq(n,1)]

comb_perms <- matrix(NA,nrow=samples*2,ncol=n)
comb_perms[seq(1,2*samples-1,by=2),] <- perms
comb_perms[seq(2,2*samples,by=2),] <- rev_perms

unique(comb_perms)[seq_len(samples),]

#### higher dim ####

n <- 15
tot_no_permutations <- factorial(n)

samples <- 10^5#factorial(n)#5*10^5

ranks = sample.int(tot_no_permutations, samples, replace = FALSE)-1

perms <- unrank_permutations_mat(ranks,n) # This is the fastest

rev_perms <- perms[,seq(n,1)]

comb_perms <- matrix(NA,nrow=samples*2,ncol=n)
comb_perms[seq(1,2*samples-1,by=2),] <- perms
comb_perms[seq(2,2*samples,by=2),] <- rev_perms

final_perms <- unique(comb_perms)[seq_len(samples),]



###############

# Number of unique permutations desired
k <- 20

# Length of the vector
n <- 4

# Total number of permutations for a list of length n
total_perm <- factorial(n)

# Generate k/2 random ranks
set.seed(123) # for reproducibility
ranks <- sample(0:(total_perm / 2 - 1), k / 2)

# Calculate ranks for the reversed permutations
rev_ranks <- total_perm - 1 - ranks

# Combine the ranks
all_ranks <- c(ranks, rev_ranks)

# Generate the permutations
permutations <- unrank_permutations(all_ranks, n)





###############

lehmer_code <- function(permutation) {
  n <- length(permutation)
  code <- rep(0, n)
  for (i in 1:n) {
    for (j in (i+1):n) {
      if (permutation[j] < permutation[i]) {
        code[i] <- code[i] + 1
      }
    }
  }
  return(code)
}

print(lehmer_code(c(4,3,1,2)))
