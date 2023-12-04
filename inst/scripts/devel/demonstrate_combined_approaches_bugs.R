# Use the data objects from the helper-lm.R file.
# Here we want to illustrate three bugs related to combined approaches (before the bugfix)


# First we see that setting `n_batches` lower than the number of unique approaches
# produce some inconsistencies in shapr.
# After the bugfix, we force the user to choose a valid value for `n_batches`.
explanation_1 = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = c("independence", "empirical", "gaussian", "copula", "empirical"),
  prediction_zero = p0,
  n_batches = 3,
  timing = FALSE,
  seed = 1)

# It says shapr is using 3 batches
explanation_1$internal$parameters$n_batches

# But shapr has actually used 4.
# This is because shapr can only handle one type of approach for each batch.
# Hence, the number of batches must be at least as large as the number of unique approaches.
# (excluding the last approach which is not used, as we then condition on all features)
length(explanation_1$internal$objects$S_batch)

# Note that after the bugfix, we give an error if `n_batches` < # unique approaches.





# Second we look at at another situation where # unique approaches is two and we set `n_batches` = 2,
# but shapr still use three batches. This is due to how shapr decides how many batches each approach
# should get. Right now it decided based on the proportion of the number of coalitions each approach
# is responsible. In this setting, independence is responsible for 5 coalitions and ctree for 25 coalitions,
# So, initially shapr sets that ctree should get the two batches while independence gets 0, but this
# is than changed to 1 without considering that it now breaks the consistency with the `n_batches`.
# This is done in the function `create_S_batch_new()` in setup_computation.R.
explanation_2 = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = c("independence", "ctree", "ctree", "ctree" ,"ctree"),
  prediction_zero = p0,
  n_batches = 2,
  timing = FALSE,
  seed = 1)

# It says shapr is using 2 batches
explanation_2$internal$parameters$n_batches

# But shapr has actually used 3
length(explanation_2$internal$objects$S_batch)

# These are equal after the bugfix


# Same type of bug but in the opposite direction
explanation_3 = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = c("independence", "ctree", "ctree", "ctree" ,"ctree"),
  prediction_zero = p0,
  n_batches = 15,
  timing = FALSE,
  seed = 1)

# It says shapr is using 15 batches
explanation_3$internal$parameters$n_batches

# It says shapr is using 14 batches
length(explanation_3$internal$objects$S_batch)

# These are equal after the bugfix






# Bug number three caused shapr to not to be reproducible as seting the seed did not work for combined approaches.
# This was due to a `set.seed(NULL)` which ruins all of the earlier set.seed procedures.


# Check that setting the seed works for a combination of approaches
# Here `n_batches` is set to `4`, so one batch for each method,
# i.e., no randomness.
# In the first example we get no bug as there is no randomness in assigning the batches.
explanation_combined_1 = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = c("independence", "empirical", "gaussian", "copula", "empirical"),
  prediction_zero = p0,
  timing = FALSE,
  seed = 1)

explanation_combined_2 = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = c("independence", "empirical", "gaussian", "copula", "empirical"),
  prediction_zero = p0,
  timing = FALSE,
  seed = 1)

# Check that they are equal
all.equal(explanation_combined_1, explanation_combined_2)


# Here `n_batches` is set to `10`, so NOT one batch for each method,
# i.e., randomness in assigning the batches.
explanation_combined_3 = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = c("independence", "empirical", "gaussian", "copula", "ctree"),
  prediction_zero = p0,
  timing = FALSE,
  seed = 1)

explanation_combined_4 = explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = c("independence", "empirical", "gaussian", "copula", "ctree"),
  prediction_zero = p0,
  timing = FALSE,
  seed = 1)

# Check that they are not equal
all.equal(explanation_combined_3, explanation_combined_4)
explanation_combined_3$internal$objects$X
explanation_combined_4$internal$objects$X

# These are equal after the bugfix

