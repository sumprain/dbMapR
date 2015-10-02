context("Queue manipulation functions")

q <- queue()

q %>% push(24) %>% push(34) %>% push(24) %>% push(12)

test_that("Queue structure is OK.", {
  expect_equal(length(q), 4L)
  expect_equal(sorted_index(q), c("1", "2", "3", "4"))
  expect_equal(counter(q), "5")
})

q %>% push(25) %>% push(36)

test_that("Queue structure is OK after inserting values more than max_length.", {
  expect_equal(length(q), 5L)
  expect_equal(sorted_index(q), c("2", "3", "4", "5", '6'))
  expect_equal(counter(q), "7")
})

remove.queue(q, "4")

test_that("removal of a given index makes queue behave normally.", {
  expect_equal(length(q), 4L)
  expect_equal(sorted_index(q), c("2", "3", "5", '6'))
  expect_equal(counter(q), "7")
})

empty(q)

test_that("after emptying a queue, it has length 0.", {
  expect_equal(length(q), 0L)
  expect_warning(remove(q, "2"))
})

q1 <- queue()
q1 %>% push(24) %>% push(12) %>% push("suman")

test_that("both queues are different.", {
  expect_false(length(q) == length(q1))
})

