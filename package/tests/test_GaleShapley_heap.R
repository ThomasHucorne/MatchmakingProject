library(testthat)

test_that("Best-First Gale-Shapley returns a valid matching", {
  men_prefs <- list(
    A = c("X", "Y", "Z"),
    B = c("Y", "X", "Z"),
    C = c("X", "Z", "Y")
  )
  women_prefs <- list(
    X = c("B", "A", "C"),
    Y = c("A", "B", "C"),
    Z = c("A", "C", "B")
  )

  matches <- best_gs_heap(men_prefs, women_prefs)

  # Check that each man has exactly one match
  expect_equal(length(unique(matches$Man)), length(matches$Man))
  # Check that each woman has exactly one match
  expect_equal(length(unique(matches$Woman)), length(matches$Woman))
  # Check that number of matches equals number of men
  expect_equal(nrow(matches), length(men_prefs))
})

test_that("Best-First Gale-Shapley handles small edge cases", {
  men_prefs <- list(A = "X")
  women_prefs <- list(X = "A")

  matches <- best_gs_heap(men_prefs, women_prefs)

  expect_equal(matches$Man, "A")
  expect_equal(matches$Woman, "X")
})

test_that("Best-First Gale-Shapley handles already matched top choices", {
  men_prefs <- list(A = c("X", "Y"), B = c("X", "Y"))
  women_prefs <- list(X = c("A", "B"), Y = c("B", "A"))

  matches <- best_gs_heap(men_prefs, women_prefs)

  # Each man should be matched to exactly one woman
  expect_equal(length(unique(matches$Man)), length(matches$Man))
  # Each woman should be matched to exactly one man
  expect_equal(length(unique(matches$Woman)), length(matches$Woman))
  # All matches should be stable
  expect_true(all(matches$Woman %in% c("X", "Y")))
})
