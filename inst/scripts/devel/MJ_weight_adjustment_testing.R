

# What we want to do is to keep the shapley_weights (after correction) from the full set of features, after removing one, removing two etc.
# In addition, we need the number of samples from each of them.
# With that info we can compute the probability of each observation being sampled at least once (call it k(x))
# then usign the shapley weights from the last iteration, we can use the same approach as in cond_all_paired

# I am not 100% sure whether the initial probabilites should sum the weights from teh two sets that CAN produce it, or just the one we used, a mean or so?



#### Just some old unusable code below here ####

org <- shapr:::exact_coalition_table(5)[]
org[,coalitions_char := unlist(lapply(coalitions, function(x) paste(x, collapse = "_")))]
org[,coalitions_new := lapply(coalitions, function(x) x[!(x %in% 5)])]
org[,coalitions_new_char := unlist(lapply(coalitions_new, function(x) paste(x, collapse = "_")))]
#org[,shapley_weight_new := sum(shapley_weight)/shapr:::sum_shapley_weights(5),by=coalitions_new_char]
org[,shapley_weight_new := sum(shapley_weight),by=coalitions_new_char]
org[coalitions_new_char=="",shapley_weight_new := 10^6]
org[coalitions_new_char==paste0(1:4,collapse="_"),shapley_weight_new := 10^6]


org_unique <- unique(org[,.(coalitions_new_char,
                            shapley_weight_new)])

new <- shapr:::exact_coalition_table(4)[]
#new[-c(1,.N),shapley_weight:=shapley_weight/shapr:::sum_shapley_weights(4)]
new[-c(1,.N),shapley_weight:=shapley_weight]

unique(org[,.(coalitions_new_char,
              shapley_weight_new)])[-c(1,.N),sum(shapley_weight_new)]

new[-c(1,.N),sum(shapley_weight)]


new[,.(coalitions,shapley_weight)]

unique(org[,.(coalitions_new_char,
              shapley_weight_new)])

new[-c(1,.N),.(coalitions,shapley_weight)][,shapley_weight/sum(shapley_weight)]

org_unique[-c(1,.N),.(coalitions_new_char,
              shapley_weight_new)][,shapley_weight_new/sum(shapley_weight_new)]



plot(org_unique[-c(1,.N),shapley_weight_new],ylim=c(0,0.3))
lines(new[-c(1,.N),shapley_weight],col=2)

n_shapley_values = 5
n_coalitions = 20
paired_shap_sampling = TRUE

shapley_reweighting = "none"
shapley_reweighting = "on_all_cond_paired"


set.seed(123)

X <- create_coalition_table(
  m = n_shapley_values,
  exact = FALSE,
  n_coalitions = n_coalitions,
  paired_shap_sampling = paired_shap_sampling,
  prev_coal_samples = NULL
)

shapr:::shapley_reweighting(X, reweight = shapley_reweighting) # Reweights the shapley weights in X by reference


S_mapper = data.table(
  feature_numbers = seq(n_shapley_values),
  feature_names = paste0(seq(n_shapley_values))
)


# Storing the feature samples
repetitions <- X[-c(1, .N), sample_freq]

unique_coal_samples <- X[-c(1, .N), coalitions]

coal_samples_org <- unlist(
  lapply(
    seq_along(unique_coal_samples),
    function(i) {
      rep(
        list(unique_coal_samples[[i]]),
        repetitions[i]
      )
    }
  ),
  recursive = FALSE
)


exclude_feature = 4

next_S_mapper <- S_mapper[-exclude_feature, ]

X_org <- copy(X)   # Should this be X og X_curr in new code?
Xtmp = X_org[, .(coalitions, id_coalition, coalition_size, sample_freq,shapley_weight)]

m <- X[.N,coalition_size]

Xtmp[,coalitions_bar:=lapply(coalitions, function(x) seq(m)[-x])]
Xtmp[1, coalitions_bar := list(1:m)]

Xtmp[, coalitions_next := lapply(coalitions, function(x) x[!(x %in% exclude_feature)])]
Xtmp[, coalitions_next := sapply(coalitions_next, match, next_S_mapper$feature_numbers)]
Xtmp[, coalitions_next_char := sapply(coalitions_next, paste0, collapse = "_")]
Xtmp[, coalitions_char := sapply(coalitions, paste0, collapse = "_")]

Xtmp[, coalitions_bar_next := lapply(coalitions_bar, function(x) x[!(x %in% exclude_feature)])]
Xtmp[, coalitions_bar_next := sapply(coalitions_bar_next, match, next_S_mapper$feature_numbers)]
Xtmp[, coalitions_bar_next_char := sapply(coalitions_bar_next, paste0, collapse = "_")]
Xtmp[, coalitions_bar_char := sapply(coalitions_bar, paste0, collapse = "_")]

setorder(Xtmp,id_coalition )
Xtmp[, keep := !duplicated(coalitions_next_char)]

Xtmp[, id_coalition_next := .GRP, by = coalitions_next_char]

# If any of the reduced coalitions are equal to the full and zero coalition,
# we remove these, and keep the original zero and full coalition
id_coalition_next_keepers <- Xtmp[c(1, .N), id_coalition_next]
Xtmp[id_coalition_next %in% id_coalition_next_keepers, keep := FALSE]
Xtmp[c(1, .N), keep := TRUE]

Xtmp[-c(1, .N), sample_freq := sum(sample_freq), by = id_coalition_next]

Xtmp = Xtmp[keep == TRUE]

Xtmp = unique(Xtmp, by = c("coalitions_char", "coalitions_bar_char"))

#Xtmp[.N, coalitions_next := 1:n_shapley_values]
repetitions <- Xtmp[-c(1, .N), sample_freq]

# Have to update coal_samples since this is what will be used in the next iteration
unique_coal_samples <- Xtmp[-c(1, .N), coalitions_next]

coal_samples <- unlist(
  lapply(
    seq_along(unique_coal_samples),
    function(i) {
      rep(
        list(unique_coal_samples[[i]]),
        repetitions[i]
      )
    }
  ),
  recursive = FALSE
)


X
Xnew <- Xtmp[,.(id_coalition_old = id_coalition,
                coalitions_old = coalitions,
                shapley_weight_old = shapley_weight,
                sample_freq,
                coalitions_new = coalitions_next,
                id_coalitions_new = id_coalition_next)]
Xnew[, coalition_size_new := as.integer(sapply(coalitions_new, length))]

n_shapley_values_new = n_shapley_values - 1
coal_samp_vec <- seq(n_shapley_values_new - 1)
n <- sapply(coal_samp_vec, choose, n = n_shapley_values_new)

Xnew[-c(1,.N), N_new := n[coalition_size_new]]

Xnew[-c(1,.N), shapley_weight := shapr:::shapley_weights(m = n_shapley_values_new, N = N, n_components = coalition_size_new, weight_zero_m = 10^6)/shapr:::sum_shapley_weights(m)]

Xnew[, shapley_weight := as.numeric(sample_freq)] # Convert to double for later calculations


Xnew[,]

