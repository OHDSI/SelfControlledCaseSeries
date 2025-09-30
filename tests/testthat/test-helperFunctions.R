library(testthat)

test_that(".logSum", {
  x <- exp(seq(log(1e-15), 10, by = 0.25))
  evalFun <- function(i) {
    tryCatch({
      result <- .logSum(log(x[i %/% length(x) + 1]), log(x[i %% length(x) + 1]))
    },
    error = function(e)
      stop(sprintf("Error for i = %d", i))
    )
    gs <- log(x[i %/% length(x) + 1] + x[i %% length(x) + 1])
    return(round(result, 8) == round(gs, 8))
  }
  good <- sapply(seq_len(length(x) ^ 2 - 1), evalFun)
  expect_true(all(good))

  good <- round(.logSum(log(x), log(x)), 8) == round(log(x + x), 8)
  expect_true(all(good))
})
