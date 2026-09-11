# check_computability rejects future-batched vaeac with a serializing future plan

    Code
      check_computability(make_internal("vaeac"))
    Condition
      Error in `check_computability()`:
      ! The "vaeac" approach relies on torch external pointers that cannot be exported to separate R processes, so future "multisession" and "cluster" plans are unsupported.
      i The only parallel option is `future::plan(future::multicore)`, which is unavailable on Windows and within RStudio.
      i For sequential computation, use `future::plan(future::sequential)` or set `extra_computation_args = list(vS_batching_method = "forloop")` in `explain()`.

