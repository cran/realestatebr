if (interactive()) {
  # Essential series for real estate package (15 key indicators)
  essential_series <- c(
    190,
    192,
    432,
    433,
    20704,
    20756,
    20768,
    20914,
    21072,
    21084,
    21340,
    24364,
    28545,
    28763,
    28770
  )

  codes_bcb <- essential_series

  resutls <- list()

  for (i in seq_along(codes_bcb)) {
    t <- try(rbcb::get_series(codes_bcb[i]))

    if (inherits(t, "try-error")) {
      cli::cli_warn(c(
        "Failed to download BCB series {.val {codes_bcb[i]}}",
        "i" = "This series will be skipped"
      ))
    } else {
      resutls[[length(resutls) + 1]] <- t
    }
  }
}
