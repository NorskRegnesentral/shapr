# TODO: DELETE THIS FILE
# Just to develop some code
if (FALSE) {
  # reweighted_shapley_kernel = function(m, paired_size = 1) {
  #   # We include all coalition sizes from paired_size to m - paired_size.
  #   # So paired_size must be a number between 1 and m - 1.
  #
  #   # Get the relevant indicies
  #   rel_ind = seq(paired_size, m - paired_size)
  #
  #   # Get the total weigh of each coalition size from 1 to m - 1
  #   weight = sapply(seq(m - 1), function(i) (m - 1) / (i * (m - i)))
  #
  #   # Set the non-relevant indices to zero
  #   weight[-rel_ind] = 0
  #
  #   # Normalize the weights of the remaining coalition sizes
  #   weight = weight / sum(weight)
  #
  #   # Divide the normalized weights by the number of coalitions
  #   # of each size to get the weight of each coalition
  #   weight[rel_ind] = weight[rel_ind] / choose(m, rel_ind)
  #
  #   return(weight)
  # }

  dt_coal_determ_info <- get_dt_coal_determ_info(10)
  m <- 5
  all.equal(get_dt_coal_determ_info(m), get_dt_coal_determ_info_old(m))

  get_dt_coal_determ_info <- function(m, weight_zero_m = 10^6) {
    # Get the number of coalition sizes for S when considering S to always be the smallest of S and Sbar.
    n_coal_sizes <- as.integer(ceiling((m - 1) / 2))

    # Get the number of coalitions of each size
    n_coal_each_size <- choose(m, seq(n_coal_sizes))

    # Get the number of coalitions deterministically included when including the different coalition sizes
    n_coal_determ <- pmin(2^m, sapply(seq(n_coal_sizes + 1), function(i) 2 + 2 * sum(choose(m, seq_len(i - 1)))))

    # Get the coalition sizes to sample
    coal_sizes_sep_sample <- c(lapply(seq(n_coal_sizes), function(size) seq(size, m - size)), NA)

    # Get the (normalized) Shapley kernel weight for each coalition sizes
    coal_size_weight <- sapply(seq(m - 1), function(i) (m - 1.0) / (i * (m - i)))
    coal_size_weight <- coal_size_weight / sum(coal_size_weight)

    # Get the weights of the coalitions deterministically included
    weight_determ <- c(sapply(seq(0, n_coal_sizes - 1), function(paired_size) {
      sum(coal_size_weight[-seq(paired_size + 1, m - paired_size - 1)])
    }), 1)

    # Get the sampling probabilities for each coalition size when included the previous sizes
    coal_sizes_sep_sample_prob <- c(lapply(seq(n_coal_sizes), function(size) {
      weigts <- coal_size_weight[seq(size, m - size)]
      return(weigts / sum(weigts))
    }), NA)

    # Get the probability of sampling the most likely coalition size when including the previous sizes
    coal_size_sampling_prob_rel <- c(sapply(coal_sizes_sep_sample_prob[seq(n_coal_sizes - 1)], "[[", 1), 1)

    # Get the number of coalitions to sample to include all coalitions of the different coalition sizes
    # based on their sampling probability and adding the previously deterministically included coalitions.
    n_coal_needed <- ceiling(n_coal_each_size / coal_size_sampling_prob_rel) + n_coal_determ[seq(n_coal_sizes)]

    # Get the max number of coalitions before we include the smaller coalition size. Ensure even numbers due to pairing.
    n_coal_needed_max <- c(sapply(n_coal_needed, function(x) ifelse(x %% 2 == 0, x - 2, x - 1)), 2^m)

    # Create a data table with all relevant information
    dt_deterministic <- data.table(
      id_step = seq(n_coal_sizes + 1),
      paired_coal_size = seq(0, n_coal_sizes),
      n_coal_max = n_coal_needed_max,
      n_coal_determ = n_coal_determ,
      weight_determ = weight_determ,
      weight_sample = 1 - weight_determ,
      coal_sizes_sample = coal_sizes_sep_sample,
      coal_sizes_sample_prob = coal_sizes_sep_sample_prob
    )

    return(dt_deterministic)
  }

  get_dt_coal_determ_info_old <- function(m, weight_zero_m = 10^6) {
    # Get the number of coalition sizes for S when considering S to always be the smallest of S and Sbar.
    n_coal_sizes <- as.integer(ceiling((m - 1) / 2))

    # Get the number of coalitions sizes where |S| != |S_bar|, which we call paired coalition sizes.
    # I.e., size 1 is paired with m - 1, 2 with m - 2, etc. Note that we do not have pairing for the
    # middle coalition size when m is even, i.e., the coalition of size ceiling((m - 1) / 2)
    n_coal_sizes_paired <- as.integer(floor((m - 1) / 2))

    # The separate sizes to sample
    coal_sizes_sep_sample <- c(lapply(seq(n_coal_sizes), function(size) seq(size, m - size)), NA)

    # The separate sizes that are deterministically included
    coal_sizes_sep_deterministic <- lapply(seq(0, n_coal_sizes), function(size) c(seq(0, size), seq(m - size, m)))

    # Compute the Shapley kernel weight of the all and paired coalition sizes. Double for the paired sizes.
    coal_size_weight <- sapply(seq(m - 1), function(i) (m - 1.0) / (i * (m - i)))
    coal_size_weight_paired <- c(
      2 * coal_size_weight[seq(n_coal_sizes_paired)],
      if (n_coal_sizes > n_coal_sizes_paired) coal_size_weight[n_coal_sizes]
    )

    # Normalize the weights
    coal_size_weight <- coal_size_weight / sum(coal_size_weight)
    coal_size_weight_paired <- coal_size_weight_paired / sum(coal_size_weight_paired)

    # Extend the weight vector with the weight for the empty/grand coalition
    coal_size_weight_ext <- c(weight_zero_m, coal_size_weight, weight_zero_m)
    coal_size_weight_paired_ext <- c(weight_zero_m, coal_size_weight_paired)

    # Create a list of the weights when incrementally including the paired coalition sizes
    coal_size_weight_ext_zeroed <- lapply(seq(0, n_coal_sizes), function(paired_size) {
      coal_size_weight_ext_tmp <- coal_size_weight_ext
      if (paired_size < n_coal_sizes) coal_size_weight_ext_tmp[seq(paired_size + 2, m - paired_size)] <- 0L
      return(coal_size_weight_ext_tmp)
    })

    # Get the coalition sizes that are deterministically included
    coal_sizes_used <- lapply(coal_size_weight_ext_zeroed, function(x) seq(0, length(x) - 1)[x > .Machine$double.eps])

    # Get the weights of the coalitions deterministically included
    weight_determ <- sapply(coal_size_weight_ext_zeroed, function(weights) sum(weights[-c(1, length(weights))]))

    # Create a list with the sampling probability for each coalition size when included the previous sizes
    coal_size_sampling_prob <- c(lapply(seq(n_coal_sizes), function(size) {
      weigts <- coal_size_weight_paired[seq(size, n_coal_sizes)]
      weigts <- weigts / sum(weigts)
      return(weigts)
    }), NA)

    # Create a list with the sampling probability for each coalition size when included the previous sizes
    coal_sizes_sep_sample_prob <- c(lapply(seq(n_coal_sizes), function(size) {
      weigts <- coal_size_weight[seq(size, m - size)]
      weigts <- weigts / sum(weigts)
      return(weigts)
    }), NA)

    # Extract the first element from each entry of the list. Used to compute the number of coalitions needed
    # to sample before we expect to include all of them and can then deterministically include them.
    coal_size_sampling_prob_rel <- sapply(coal_size_sampling_prob, "[[", 1)[seq(n_coal_sizes)]

    # Get the number of coalitions deterministically included when including the different coalition sizes
    n_coal_determ <- pmin(2^m, sapply(seq(n_coal_sizes + 1), function(i) 2 + 2 * sum(choose(m, seq_len(i - 1)))))

    # n_coal_sizes = as.integer(ceiling((m - 1) / 2))
    # m
    # coal_size = 1
    # get_n_coal_determ = function(m, coal_size) {
    #   min(2^m, 2 + 2*sum(choose(m, seq_len(coal_size))))
    # }
    #
    # get_coal_size_determ(m, n_coalitions) {
    #
    #
    # }

    # Get the number of coalitions of each size
    n_coal_each_size <- choose(m, seq(n_coal_sizes))
    n_coal_each_size[seq(n_coal_sizes_paired)] <- 2 * n_coal_each_size[seq(n_coal_sizes_paired)]

    # Get the number of coalitions to sample to include all coalitions of the different coalition sizes
    # based on their sampling probability and adding the previously deterministically included coalitions.
    n_coal_needed <- ceiling(n_coal_each_size / coal_size_sampling_prob_rel) + n_coal_determ[seq(n_coal_sizes)]

    # Get the max number of coalitions before we include the smaller coalition size. Ensure even numbers due to pairing.
    n_coal_needed_max <- sapply(n_coal_needed, function(x) ifelse(x %% 2 == 0, x - 2, x - 1))
    n_coal_needed_max <- c(n_coal_needed_max, 2^m)

    # Get the reweighted Shapley kernel weight for the coalition sizes that are to be sampled
    # reweighted_shapley_weight = c(lapply(seq(0, n_coal_sizes_paired), function(paired_size){
    #
    #   # Get only the weights of the coalition sizes to be sampled
    #   coal_size_weight_now = coal_size_weight[seq(paired_size + 1, m - paired_size - 1)]
    #   coal_size_weight_now = coal_size_weight_now / sum(coal_size_weight_now)
    #
    #   # Get the number of coalitions of each size to be sampled
    #   n_coal_each_size_single_now = choose(m, seq(paired_size + 1, m - paired_size - 1))
    #
    #   # Get the Shapley kernel weight for each coalition of each size
    #   reweighted_shapley_kernel = coal_size_weight_now / n_coal_each_size_single_now
    #
    #   # Pad it with zeros
    #   reweighted_shapley_kernel = c(rep(0, paired_size), reweighted_shapley_kernel, rep(0, paired_size))
    #
    #   return(reweighted_shapley_kernel)
    # }), NA)

    reweighted_shapley_weight_arr <- c(lapply(seq(0, n_coal_sizes_paired - 1), function(paired_size) {
      reweighted_shapley_weight(m, paired_size)
    }), NA)

    # reweighted_shapley_kernel
    #
    #
    # reweighted_shapley_kernel(m, 4)
    #
    #   sw = shapley_weights(
    #          m = m,
    #          N = choose(m, seq(m-1)),
    #          n_components = seq(m-1),
    #          weight_zero_m = 10^6
    #     )
    #   sw
    #   sw = sw / sum_shapley_weights(m)
    #   sw
    #
    #   sum_shapley_weights(m)
    #   non_rel_ind = c(seq(1, max_paired_size), seq(m - max_paired_size + 1, m))
    #
    #
    #
    #   coal_size_weight / sum(coal_size_weight)
    #
    #   sum(coal_size_weight)
    #
    #   sum_shapley_weights(m)
    #
    #   max_paired_size = 3
    # Get the original Shapley kernel weight for the coalition sizes
    # coal_size_weight = sapply(seq(m-1), function(i) (m - 1.0) / (i * (m - i)))
    # coal_size_weight = coal_size_weight / sum(coal_size_weight)
    #
    # if (!is.null(max_paired_size)) {
    #
    #   # Get only the weights of the coalition sizes to be sampled
    #   coal_size_weight_now = coal_size_weight[seq(max_paired_size + 1, m - max_paired_size - 1)]
    #   coal_size_weight_now = coal_size_weight_now / sum(coal_size_weight_now)
    #   coal_size_weight_now
    #
    #   # Get the number of coalitions of each size to be sampled
    #   n_coal_each_size_single_now = choose(m, seq(max_paired_size + 1, m - max_paired_size - 1))
    #
    #   # Get the Shapley kernel weight for each coalition of each size
    #   reweighted_shapley_kernel = coal_size_weight_now / n_coal_each_size_single_now
    #
    #   # Pad it with zeros
    #   coal_size_weight = c(rep(0, max_paired_size), reweighted_shapley_kernel, rep(0, max_paired_size))
    #   coal_size_weight
    #   coal_size_weight_new
    #
    # }

    # return(coal_size_weight)



    # Create a data table to store the results
    dt_deterministic <- data.table(
      id_step = seq(n_coal_sizes + 1),
      coal_size = seq(0, n_coal_sizes),
      n_coal_max = n_coal_needed_max,
      n_coal_determ = n_coal_determ[seq(n_coal_sizes + 1)],
      weight_determ = weight_determ,
      weight_sample = 1 - weight_determ,
      paired_sizes_to_sample = c(sapply(seq(0, n_coal_sizes - 1), function(i) seq(1 + i, ceiling((m - 1) / 2))), list(NA)),
      paired_sizes_to_sample_prob = coal_size_sampling_prob,
      paired_size_weights_paired = sapply(seq(0, n_coal_sizes), function(i) coal_size_weight_paired_ext[seq(i + 1)]),
      size_weights = coal_size_weight_ext_zeroed,
      coal_sizes_used = coal_sizes_used,
      reweighted_shapley_weight_arr = reweighted_shapley_weight_arr,
      coal_sizes_sep_deterministic = coal_sizes_sep_deterministic,
      coal_sizes_sep_sample = coal_sizes_sep_sample,
      coal_sizes_sep_sample_prob = coal_sizes_sep_sample_prob
    )

    dt_deterministic <- data.table(
      id_step = seq(n_coal_sizes + 1),
      paired_coal_size = seq(0, n_coal_sizes),
      n_coal_max = n_coal_needed_max,
      n_coal_determ = n_coal_determ[seq(n_coal_sizes + 1)],
      weight_determ = weight_determ,
      weight_sample = 1 - weight_determ,
      coal_sizes_sample = coal_sizes_sep_sample,
      coal_sizes_sample_prob = coal_sizes_sep_sample_prob
    )



    return(dt_deterministic)
  }

  #' Title
  #'
  #' @param m
  #' @param n_combinations
  #'
  #' @returns
  #' @export
  #'
  #' @examples
  n_coalitions_kernelSHAP <- function(m, weight_zero_m = 10^6) {
    m

    # Compute the Shapley kernel weight of the paired coalition sizes
    num_subset_sizes <- as.integer(ceiling((m - 1) / 2))
    num_paired_subset_sizes <- as.integer(floor((m - 1) / 2))
    weight_vector <- sapply(seq(num_subset_sizes), function(i) (m - 1.0) / (i * (m - i)))
    weight_vector[seq(num_paired_subset_sizes)] <- 2 * weight_vector[seq(num_paired_subset_sizes)]
    weight_vector <- weight_vector / sum(weight_vector)
    weight_vector_extended <- c(weight_zero_m, weight_vector)

    # Compute the Shapley kernel weight of the paired coalition sizes
    weight <- sapply(seq(m - 1), function(i) (m - 1.0) / (i * (m - i)))
    weight <- weight / sum(weight)

    weight_extended <- c(weight_zero_m, weight, weight_zero_m)


    all_weights_padded <- lapply(seq(0, num_paired_subset_sizes), function(size) {
      weight_extended_pad <- weight_extended
      weight_extended_pad[seq(subset_size + 2, m - subset_size)] <- 0
      weight_extended_pad
    })
    if (num_subset_sizes > num_paired_subset_sizes) all_weights_padded <- c(all_weights_padded, list(weight_extended))
    all_weights_padded



    arrays_list



    # Half the weight of the unpaired coalition size, i.e., coalition size where |S| = |S_bar|.
    if (m %% 2 == 0) weight[num_paired_subset_sizes + 1] <- weight[num_paired_subset_sizes + 1] / 2


    # Variable that will store the normalized probability of sampling the remaining coalition sizes
    remaining_weight_vector <- copy(weight_vector)
    remaining_weight_vector_list <- list(remaining_weight_vector)

    # Array to store the number of combinations needed to include the different coalition sizes
    n_comb_needed <- NULL

    # Find the number of combinations needed to include the different coalition sizes
    subset_size <- 1
    for (subset_size in seq(num_subset_sizes)) {
      # Get the number of (paired) coalitions of this subset size
      nsubsets <- choose(m, subset_size)
      if (subset_size <= num_paired_subset_sizes) nsubsets <- 2 * nsubsets

      # Get the expected number of samples needed to sample nsubsets coalitions of size
      # `subset_size` using the normalized sampling probability
      n_comb_needed_now <- ceiling(nsubsets / remaining_weight_vector[subset_size]) + 2 # EMPTY and grand coaltions

      # Add the number of coalitions of smaller sizes that are included
      if (subset_size > 1) n_comb_needed_now <- n_comb_needed_now + 2 * sum(choose(m, seq(subset_size - 1)))

      # Store the new values
      n_comb_needed <- c(n_comb_needed, n_comb_needed_now)

      # Update the probability of the remaining coalition sizes such that they sum to 1
      if (remaining_weight_vector[subset_size] < 1.0) {
        remaining_weight_vector <- remaining_weight_vector / (1 - remaining_weight_vector[subset_size])
        remaining_weight_vector_list <- c(remaining_weight_vector_list, list(remaining_weight_vector[-(1:subset_size)]))
      }
    }
    n_comb_needed

    # Create a data table with max number of coalitions before we include the smaller coalition size. Ensure even numbers
    n_comb_needed <- sapply(n_comb_needed, function(x) ifelse(x %% 2 == 0, x - 2, x - 1))
    n_comb_needed <- c(n_comb_needed, 2^m)

    # n_comb_needed[n_comb_needed >= n_combinations] = n_combinations
    # n_comb_needed[length(n_comb_needed)] = n_combinations
    dt_n_comb_needed <- data.table(id_step = seq_along(n_comb_needed))
    dt_n_comb_needed[, paired_sizes_included := id_step - 1L]
    dt_n_comb_needed[, max_n_coalitions := n_comb_needed]
    dt_n_comb_needed[, n_coalitions_fixed := pmin(2^m, 2 + 2 * sapply(id_step, function(id) sum(choose(m, seq_len(id - 1)))))]
    dt_n_comb_needed[, weights_paired_coalitions_fixed := sapply(paired_sizes_included, function(i) weight_vector_extended[seq(i + 1)])]
    dt_n_comb_needed[, interior_weight_determ := sapply(weights_paired_coalitions_fixed, function(i) sum(i[-1]))]
    dt_n_comb_needed[, interior_weight_sample := 1 - interior_weight_determ]
    dt_n_comb_needed[-.N, sizes_to_sample := sapply(paired_sizes_included, function(i) seq(1 + i, ceiling((m - 1) / 2)))]
    dt_n_comb_needed[.N, sizes_to_sample := NA]
    dt_n_comb_needed[, sizes_to_sample_weights := c(remaining_weight_vector_list, NA)]
    dt_n_comb_needed[, all_weights_padded := all_weights_padded]
    dt_n_comb_needed

    dt_n_comb_needed

    dt_8 <- exact_coalition_table(8)
    dt_8[-c(1, .N), shapley_weight_2 := shapley_weight / sum(shapley_weight)]
    dt_8
    weight / choose(8, seq(8 - 1))
    dt_8[, unique(shapley_weight_2)]

    id_step_now <- 3
    coal_samp_vec <- dt_n_comb_needed[id_step == id_step_now, sizes_to_sample[[1]]]
    p <- dt_n_comb_needed[id_step == id_step_now, sizes_to_sample_weights[[1]]]
    n_samps <- 100000

    # Sample the coalition sizes
    coal_size_sample <- sample(x = coal_samp_vec, size = n_samps, replace = TRUE, prob = p)
    # Sample the (paired) coalitions as strings
    coalitions <- sample_coalitions_cpp_str_paired(m, coal_size_sample, paired_shap_sampling = TRUE)
    coalitions
    freq <- table(sapply(coalitions, function(s) stringi::stri_count_fixed(s, " ")))
    freq <- freq / sum(freq)
    c(freq[1] + freq[3], freq[2])

    DT_deterministic[-c(1, .N), shapley_weight := shapley_weight * 0.559229 / sum(shapley_weight)]
    DT_deterministic[-c(1, .N), sum(shapley_weight)]


    # Add the remaining weight vector

    # Logic:
    # Check how many fixed coalitions we can include.
    # Include any if we can.
    # Remove the fixed from the sampled coalitions.
    # If the new sum of fixed + previous sampled is larger than the new value (in case the fixed were not sampled)
    # then we do not need to sample anything and we essentially remove some of the sampled.
    # Otherwise Start sampling the new coalitions.
    # When we have enough coalitions.
    # Combine the DT_deterministic and DT_sampled.
    # Then add option for c-kernel on DT_smapled.
    # Update the weights such that the DT_determ and DT_sampled repsesnt the correct amount.
    # THen the rest should go by itself.




    return(dt_n_comb_needed)
  }



  m <- 10
  DT_n_coalitions <- n_coalitions_kernelSHAP(10)
  DT_n_coalitions
  n_coalitions_now <- 400

  max_paired_coal_size_include <- DT_n_coalitions[which.max(n_coalitions >= n_coalitions_now), id - 1]



  m <- 8
  DT_deterministic <- exact_coalition_table_determ(m, 3)
  DT_deterministic2 <- exact_coalition_table(m, 3)
  all.equal(DT_deterministic2, DT_deterministic)
  dt[-c(1, .N), shapley_weight := shapley_weight / sum(shapley_weight)]
  DT_deterministic

  dt_exact <- exact_coalition_table(m, dt_valid_causal_coalitions = NULL, weight_zero_m = 10^6)
  dt_exact[-c(1, .N), shapley_weight := shapley_weight / sum(shapley_weight)]
  dt_exact

  all.equal(dt_exact, DT_deterministic)
  dt_exact[, shapley_weight] - DT_deterministic[, shapley_weight]


  exact_coalition_table_determ <- function(m, max_paired_coal_size_include, weight_zero_m = 10^6) {
    # Small check for valid input
    if (max_paired_coal_size_include > as.integer(ceiling((m - 1) / 2))) {
      stop(paste0(
        "The argument `max_paired_coal_size_include` (", max_paired_coal_size_include, ") is larger ",
        "than the number of paired coalition sizes (", as.integer(ceiling((m - 1) / 2)), ")."
      ))
    }

    # Get the sizes of the coalitions that are included, including the paired sizes.
    # Use unique such that the an coalition size where |S| = |S_bar| is not included twice.
    seq_coal_size_include <- unique(c(seq(0, max_paired_coal_size_include), m - seq(max_paired_coal_size_include, 0)))

    # Get all the relevant coalition sizes
    coalitions0 <- unlist(lapply(seq_coal_size_include, utils::combn, x = m, simplify = FALSE), recursive = FALSE)

    # Create a data table to store the results
    dt <- data.table::data.table(id_coalition = seq_along(coalitions0))
    dt[, coalitions := coalitions0]
    dt[, coalitions_str := sapply(coalitions, paste, collapse = " ")]
    dt[, coalition_size := lengths(coalitions), id_coalition]
    dt[, N := choose(m, coalition_size)]
    dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = coalition_size, weight_zero_m)]
    dt[, sample_freq := NA]

    return(dt)
  }


  exact_coalition_table_determ_old <- function(m, max_paired_coal_size_include, weight_zero_m = 10^6) {
    if (max_paired_coal_size_include > as.integer(ceiling((m - 1) / 2))) {
      stop("The maximum number of paired coalition sizes to include is larger than the number of paired coalition sizes.")
    }

    # Get the number of coalitions with paired sizes, i.e., the coalitions sizes where |S| != |S_bar|.
    n_coalition_paired_sizes <- as.integer(floor((m - 1) / 2))

    # Compute the Shapley kernel weight of the paired coalition sizes
    weight <- sapply(seq(m - 1), function(i) (m - 1.0) / (i * (m - i)))
    weight <- weight / sum(weight)

    # Half the weight of the unpaired coalition size, i.e., coalition size where |S| = |S_bar|.
    # if (m %% 2 == 0) weight[num_paired_subset_sizes + 1] = weight[num_paired_subset_sizes + 1]/2

    # Get the sizes of the coalitions that are included, including the paired sizes.
    # Use unique such that the an coalition size where |S| = |S_bar| is not included twice.
    seq_coal_size_include <- unique(c(seq(0, max_paired_coal_size_include), m - seq(max_paired_coal_size_include, 0)))

    # Get all the relevant coalition sizes
    coalitions0 <- unlist(lapply(seq_coal_size_include, utils::combn, x = m, simplify = FALSE), recursive = FALSE)

    # Create a data table to store the results
    dt <- data.table::data.table(id_coalition = seq_along(coalitions0))
    dt[, coalitions := coalitions0]
    dt[, coalitions_str := sapply(coalitions, paste, collapse = " ")]
    dt[, coalition_size := lengths(coalitions), id_coalition]
    dt[, N := choose(m, coalition_size)]
    dt[c(1, .N), shapley_weight := weight_zero_m]
    dt[-c(1, .N), shapley_weight := weight[coalition_size] / N]
    dt[-c(1, .N), shapley_weight := shapley_weight / sum(shapley_weight)]
    dt[, shapley_weight2 := shapley_weights(m = m, N = N, n_components = coalition_size, weight_zero_m)]
    dt[-c(1, .N), shapley_weight2 := shapley_weight2 / sum(shapley_weight2)]
    dt[, sample_freq := NA]
    dt

    dt[-c(1, .N), sum(shapley_weight), coalition_size]

    return(dt)

    # coalition_size = 2
    # shapley_weights(m = m, N = choose(m, coalition_size), n_components = coalition_size, weight_zero_m)*choose(m, coalition_size)
    #
    #
    # dt_res[, sum(w)]
    #
    # weight_vector = sapply(seq(num_subset_sizes), function(i) (m - 1.0) / (i * (m - i)))
    # weight_vector[seq(num_paired_subset_sizes)] = 2*weight_vector[seq(num_paired_subset_sizes)]
    # weight_vector = weight_vector / sum(weight_vector)
    # weight_vector
    # 2*weight
    #
    # # data table to store the coalitions that are pre-defined
    # # to be included with the corresponding normalized Shapley kernel weights
    # dt_res = NULL

    # # For all `max_paired_coal_size_include` larger than 1,
    # # we include all coalitions of sizes less than `max_paired_coal_size_include`
    # if (max_paired_coal_size_include > 0) {
    #
    #   subset_size = 1
    #   for (subset_size in seq(max_paired_coal_size_include)) {
    #     feature_sample = unlist(lapply(subset_size, utils::combn, x = m, simplify = FALSE), recursive = FALSE)
    #     w = weight_vector[subset_size] / choose(m, subset_size)
    #     if (subset_size <= num_paired_subset_sizes) {
    #       # Add paired sampled and half the weight
    #       feature_sample = c(rbind(feature_sample, lapply(feature_sample, function(x, m) {seq(m)[-x]}, m = m)))
    #       w = w / 2
    #     }
    #     dt_res_now = data.table(features = feature_sample, w = w)
    #     dt_res = rbind(dt_res, dt_res_now)
    #   }
    # }
    # dt_res = dt_res[order(lengths(features))]
    #
    # seq_all = c(seq(max_paired_coal_size_include), m - seq(max_paired_coal_size_include, 1))
    # # seq_paired = seq_all[seq_all <= num_paired_subset_sizes]
    # # weight_vector[seq_paired] / (2*choose(m, seq_paired))
    #
    # coalitions0 <- unlist(lapply(seq_all, utils::combn, x = m, simplify = FALSE), recursive = FALSE)
    #
    #
    # # ll = unlist(lapply(seq_paired, utils::combn, x = m, simplify = FALSE), recursive = FALSE)
    # # ll = c(ll, ll_unpaired lapply(ll, function(x, m) {seq(m)[-x]}, m = m))
    # #
    # #
    # # coalitions0 <- unlist(lapply(0:1, utils::combn, x = m, simplify = FALSE), recursive = FALSE)
  }

  #' @keywords internal
  kernelSHAP_reweighting_old <- function(X, reweight = "on_N") {
    # Updates the Shapley weights in X based on the reweighting strategy BY REFERENCE

    if (reweight == "on_N") {
      X[-c(1, .N), shapley_weight := mean(shapley_weight), by = N]
    } else if (reweight == "on_all") {
      m <- X[.N, coalition_size]
      X[-c(1, .N), shapley_weight := shapley_weights(
        m = m,
        N = N,
        n_components = coalition_size,
        weight_zero_m = 10^6
      ) / sum_shapley_weights(m)]
    } else if (reweight == "on_all_cond") {
      m <- X[.N, coalition_size]
      K <- X[, sum(sample_freq)]
      X[-c(1, .N), shapley_weight := shapley_weights(
        m = m,
        N = N,
        n_components = coalition_size,
        weight_zero_m = 10^6
      ) / sum_shapley_weights(m)]
      X[-c(1, .N), cond := 1 - (1 - shapley_weight)^K]
      X[-c(1, .N), shapley_weight := shapley_weight / cond]
    }
    # strategy= "none" or something else do nothing
    return(NULL)
  }


  #' @keywords internal
  kernelSHAP_reweighting_determ <- function(X, reweight = "on_N", m = NULL, reweighted_shapley_weight = NULL) {
    # Updates the Shapley weights in X based on the reweighting strategy BY REFERENCE
    # Only used for semi-deterministic sampling, i.e., this function is slightly
    # different from kernelSHAP_reweighting

    if (is.null(reweighted_shapley_weight)) {
      # It is null for random sampling
      m <- X[.N, coalition_size] # Assume last row is the full coalition
      rel_ind <- -c(1, nrow(X))
      func_shapley_weights <- function(m, N, coalition_size, weight_zero_m = 10^6) {
        shapley_weights(m = m, N = N, n_components = coalition_size, weight_zero_m = weight_zero_m) /
          sum_shapley_weights(m)
      }
    } else {
      # We are doing semi-deterministic sampling
      rel_ind <- seq_len(nrow(X))
      if (is.null(m)) stop("`m` must be provided for semi-deterministic sampling")
      func_shapley_weights <- function(m, N, coalition_size, weight_zero_m = 10^6) {
        reweighted_shapley_weight[coalition_size]
      }
    }

    # Do the reweighting
    if (reweight == "on_N") {
      X[rel_ind, shapley_weight := mean(shapley_weight), by = N]
    } else if (reweight == "on_all") {
      X[rel_ind, shapley_weight := func_shapley_weights(m = m, N = N, coalition_size = coalition_size)]
    } else if (reweight == "on_all_cond") {
      K <- X[, sum(sample_freq)]
      X[rel_ind, shapley_weight := func_shapley_weights(m = m, N = N, coalition_size = coalition_size)]
      X[rel_ind, cond := 1 - (1 - shapley_weight)^K]
      X[rel_ind, shapley_weight := shapley_weight / cond]
    }
    # strategy= "none" or something else do nothing
    return(NULL)
  }
}
# Logic:
# Check how many fixed coalitions we can include.
# Include any if we can.
# Remove the fixed from the sampled coalitions.
# If the new sum of fixed + previous sampled is larger than the new value (in case the fixed were not sampled)
# then we do not need to sample anything and we essentially remove some of the sampled.
# Otherwise Start sampling the new coalitions.
# When we have enough coalitions.
# Combine the DT_deterministic and DT_sampled.
# Then add option for c-kernel on DT_smapled.
# Update the weights such that the DT_determ and DT_sampled repsesnt the correct amount.
# THen the rest should go by itself.
