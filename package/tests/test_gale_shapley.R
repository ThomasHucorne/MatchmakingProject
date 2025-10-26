test_that("Gale–Shapley returns a valid matching", {
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

  matches <- gale_shapley(men_prefs, women_prefs)

  # Check that we have one unique match per man and woman
  expect_true(all(unique(matches$Man) == matches$Man))
  expect_true(all(unique(matches$Woman) == matches$Woman))
  expect_equal(nrow(matches), length(men_prefs))
})

test_that("Algorithm handles small edge cases", {
  men_prefs <- list(A = "X")
  women_prefs <- list(X = "A")
  matches <- gale_shapley(men_prefs, women_prefs)

  expect_equal(matches$Man, "A")
  expect_equal(matches$Woman, "X")
})
