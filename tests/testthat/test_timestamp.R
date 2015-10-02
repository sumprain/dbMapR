context("Testing timestamp behaviour")

t1 <- cur_timestamp(digits = 1L)
t2 <- cur_timestamp(digits = 1L)
t3 <- cur_timestamp(digits = 1L,tz = "UTC")

test_that("Relative time difference holds good.", {
  expect_true(t1 %earlier% t2 | t1 %same_time% t2)
  expect_true(t1 %earlier% t3)
})
