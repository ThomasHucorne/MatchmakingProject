test_that("Gale–Shapley returns a valid matching (R version)", {
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

test_that("Gale–Shapley returns a valid matching (C++ version)", {
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
  matches <- gale_shapley_cpp(men_prefs, women_prefs)
  # Check that we have one unique match per man and woman
  expect_true(all(unique(matches$Man) == matches$Man))
  expect_true(all(unique(matches$Woman) == matches$Woman))
  expect_equal(nrow(matches), length(men_prefs))
})

test_that("Algorithm handles small edge cases (R version)", {
  men_prefs <- list(A = "X")
  women_prefs <- list(X = "A")
  matches <- gale_shapley(men_prefs, women_prefs)
  expect_equal(matches$Man, "A")
  expect_equal(matches$Woman, "X")
})

test_that("Algorithm handles small edge cases (C++ version)", {
  men_prefs <- list(A = "X")
  women_prefs <- list(X = "A")
  matches <- gale_shapley_cpp(men_prefs, women_prefs)
  expect_equal(matches$Man, "A")
  expect_equal(matches$Woman, "X")
})

test_that("R and C++ versions produce identical results", {
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

  matches_r <- gale_shapley(men_prefs, women_prefs)
  matches_cpp <- gale_shapley_cpp(men_prefs, women_prefs)

  # Sort both by Woman for comparison
  matches_r <- matches_r[order(matches_r$Woman), ]
  matches_cpp <- matches_cpp[order(matches_cpp$Woman), ]

  # Reset row names for comparison
  rownames(matches_r) <- NULL
  rownames(matches_cpp) <- NULL

  expect_equal(matches_r, matches_cpp)
})

test_that("Both versions handle larger instances", {
  men_prefs <- list(
    A = c("W", "X", "Y", "Z"),
    B = c("X", "Y", "Z", "W"),
    C = c("Y", "Z", "W", "X"),
    D = c("Z", "W", "X", "Y")
  )
  women_prefs <- list(
    W = c("B", "A", "C", "D"),
    X = c("A", "B", "C", "D"),
    Y = c("A", "B", "C", "D"),
    Z = c("A", "B", "C", "D")
  )

  matches_r <- gale_shapley(men_prefs, women_prefs)
  matches_cpp <- gale_shapley_cpp(men_prefs, women_prefs)

  # Both should produce valid matchings
  expect_equal(nrow(matches_r), 4)
  expect_equal(nrow(matches_cpp), 4)
  expect_true(all(unique(matches_r$Man) == matches_r$Man))
  expect_true(all(unique(matches_cpp$Man) == matches_cpp$Man))

  # And they should be identical
  matches_r <- matches_r[order(matches_r$Woman), ]
  matches_cpp <- matches_cpp[order(matches_cpp$Woman), ]
  rownames(matches_r) <- NULL
  rownames(matches_cpp) <- NULL
  expect_equal(matches_r, matches_cpp)
})
