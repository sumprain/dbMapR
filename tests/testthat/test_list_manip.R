context("List manipulation functions in utils.R")

# make a list to be checked ------------------------

l <- list(a = c(1:3), b = letters[1:6], c = rnorm(10))

val <- letters[10:18]

id <- "aaaa"

lim <- 3

# running remove_from_list -------------------------

rm_list <- remove_from_list(l)

testthat::test_that("First item is removed from list.", {
  testthat::expect_equal(rm_list, l[-1])
})

# running add_val_to_list -----------------------------

add_list <- add_val_to_list(l, val, id, lim)

testthat::test_that("Val is added to list with id.", {
  testthat::expect_equal(names(add_list), c("b", "c", "aaaa"))
  testthat::expect_equal(add_list[["aaaa"]], letters[10:18])
})

# running empty_list ----------------------------------

e_list <- empty_list(l)

testthat::test_that("List is emptied successfully.", {
  testthat::expect_is(e_list, "list")
  testthat::expect_equal(length(e_list), 0)
})
