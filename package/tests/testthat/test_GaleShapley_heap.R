library(testthat)

test_that("Best-First Gale-Shapley returns a valid matching", {
  men_prefs_1 <- list(
    A = c("X", "Y", "Z"),
    B = c("Y", "X", "Z"),
    C = c("X", "Z", "Y")
  )
  women_prefs_1 <- list(
    X = c("B", "A", "C"),
    Y = c("A", "B", "C"),
    Z = c("A", "C", "B")
  )

  matches_1 <- best_gs_heap(men_prefs_1, women_prefs_1)

  # Check that each man has exactly one match
  expect_equal(length(unique(matches_1$Man)), length(matches_1$Man))
  # Check that each woman has exactly one match
  expect_equal(length(unique(matches_1$Woman)), length(matches_1$Woman))
  # Check that number of matches equals number of men
  expect_equal(nrow(matches_1), length(men_prefs_1))
})

test_that("Best-First Gale-Shapley handles small edge cases", {
  men_prefs_2 <- list(A = "X")
  women_prefs_2 <- list(X = "A")

  matches_2 <- best_gs_heap(men_prefs_2, women_prefs_2)

  expect_equal(matches_2$Man, "A")
  expect_equal(matches_2$Woman, "X")
})

test_that("Best-First Gale-Shapley handles already matched top choices", {
  men_prefs_3 <- list(A = c("X", "Y"), B = c("X", "Y"))
  women_prefs_3 <- list(X = c("A", "B"), Y = c("B", "A"))

  matches_3 <- best_gs_heap(men_prefs_3, women_prefs_3)

  # Each man should be matched to exactly one woman
  expect_equal(length(unique(matches_3$Man)), length(matches_3$Man))
  # Each woman should be matched to exactly one man
  expect_equal(length(unique(matches_3$Woman)), length(matches_3$Woman))
  # All matches should be stable
  expect_true(all(matches_3$Woman %in% c("X", "Y")))
  expect_true(all(matches_3$Man %in% c("A", "B")))
})

test_that("C++ Gale–Shapley handles fully opposite preferences", {
  men_prefs_4 <- list(
    A = c("X", "Y", "Z"),
    B = c("X", "Y", "Z"),
    C = c("X", "Y", "Z")
  )

  women_prefs_4 <- list(
    X = c("C", "B", "A"),
    Y = c("C", "A", "B"),
    Z = c("B", "A", "C")
  )

  matches_4 <- best_gs_heap(men_prefs_4, women_prefs_4)

  expect_equal(nrow(matches_4), 3)
  expect_equal(length(unique(matches_4$Man)), 3)
  expect_equal(length(unique(matches_4$Woman)), 3)
})

test_that("C++ Gale–Shapley handles cyclic preference structures", {
  men_prefs_5 <- list(
    A = c("X", "Y", "Z"),
    B = c("Y", "Z", "X"),
    C = c("Z", "X", "Y")
  )

  women_prefs_5 <- list(
    X = c("B", "C", "A"),
    Y = c("C", "A", "B"),
    Z = c("A", "B", "C")
  )

  matches_5 <- best_gs_heap(men_prefs_5, women_prefs_5)

  expect_equal(nrow(matches_5), 3)
  expect_equal(length(unique(matches_5$Man)), 3)
  expect_equal(length(unique(matches_5$Woman)), 3)
})

test_that("C++ Gale–Shapley handles nearly identical preferences", {
  men_prefs_6 <- list(
    A = c("Y", "X", "Z"),
    B = c("Y", "Z", "X"),
    C = c("Y", "X", "Z")
  )

  women_prefs_6 <- list(
    X = c("A", "B", "C"),
    Y = c("A", "C", "B"),
    Z = c("C", "A", "B")
  )

  matches_6 <- best_gs_heap(men_prefs_6, women_prefs_6)

  expect_equal(nrow(matches_6), 3)
  expect_equal(length(unique(matches_6$Man)), 3)
  expect_equal(length(unique(matches_6$Woman)), 3)
})
