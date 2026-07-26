# check_computability warns when vaeac is combined with a serializing future plan

    Code
      check_computability(make_internal("vaeac"))
    Condition
      Warning:
      ! The "vaeac" approach uses torch models, whose objects are external pointers that cannot be exported to future "multisession"/"cluster" workers.
      x This will fail with an 'external pointer is not valid' error during the v(S) computation.
      i Use a forking plan instead (`future::plan(future::multicore)`) or run sequentially.
        Forking is unavailable on Windows and within RStudio; run "vaeac" sequentially there.

