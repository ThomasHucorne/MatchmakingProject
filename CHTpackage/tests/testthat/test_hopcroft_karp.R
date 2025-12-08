# ==============================================================================
# Test script for R version of Hopcroft-Karp Algorithm
# ==============================================================================
# This script tests the original R implementation
# Run this after sourcing the original R file

# Source the original R implementation
# source("hopcroft_karp_original.R")

library(testthat)

# ==============================================================================
# Test 1: Basic functionality with small dataset
# ==============================================================================
test_that("R version - Basic functionality works", {
  cat("\n=== TEST 1: Basic functionality ===\n")

  set.seed(42)
  data <- data.frame(
    id = 1:20,
    blood_type = c("O-", "O+", "A-", "A+", "B-", "B+", "AB-", "AB+",
                   "O-", "O+", "A-", "A+", "B-", "B+", "AB-", "AB+",
                   "O+", "A+", "B+", "AB+")
  )

  donors <- c(1, 2, 3, 4, 5, 6, 7, 8)
  receivers <- c(9, 10, 11, 12, 13, 14, 15, 16)

  compatibility_table <- create_compatibility_table()

  result <- hopcroft_karp(donors, receivers, data, compatibility_table, can_receive)

  expect_true(is.list(result))
  expect_true("matching_donor" %in% names(result))
  expect_true("matching_receiver" %in% names(result))
  expect_true("matching_size" %in% names(result))
  expect_gte(result$matching_size, 0)
  expect_lte(result$matching_size, min(length(donors), length(receivers)))

  cat(sprintf("✓ Matching size: %d/%d\n", result$matching_size, length(donors)))
})

# ==============================================================================
# Test 2: Universal donor O- can donate to everyone
# ==============================================================================
test_that("R version - Universal donor O- compatibility", {
  cat("\n=== TEST 2: Universal donor O- ===\n")

  data <- data.frame(
    id = 1:14,
    blood_type = c("O-", "O-", "O-", "O-", "O-", "O-", "O-", "A+", "A-", "B+", "B-", "AB+", "AB-", "O+")
  )

  donors <- 1:7 # O- donor
  receivers <- 8:14  # All other types

  compatibility_table <- create_compatibility_table()

  result <- hopcroft_karp(donors, receivers, data, compatibility_table, can_receive)

  expect_equal(result$matching_size, 7)
  expect_false(is.na(result$matching_donor[["1"]]))

  receiver_id <- result$matching_donor[["1"]]
  cat(sprintf("✓ O- donor matched with receiver %d (%s)\n",
              receiver_id, data$blood_type[receiver_id]))
})

# ==============================================================================
# Test 3: Universal recipient AB+ can receive from everyone
# ==============================================================================
test_that("R version - Universal recipient AB+ compatibility", {
  cat("\n=== TEST 3: Universal recipient AB+ ===\n")

  data <- data.frame(
    id = 1:9,
    blood_type = c("AB+", "O+", "A+", "A-", "B+", "B-", "AB-", "O-", "B+")
  )

  donors <- c(2, 3, 4, 5, 6, 7, 8)  # Various donor types
  receivers <- c(1)  # AB+ receiver

  compatibility_table <- create_compatibility_table()

  result <- hopcroft_karp(donors, receivers, data, compatibility_table, can_receive)

  expect_equal(result$matching_size, 1)
  expect_false(is.na(result$matching_receiver[["1"]]))

  donor_id <- result$matching_receiver[["1"]]
  cat(sprintf("✓ AB+ receiver matched with donor %d (%s)\n",
              donor_id, data$blood_type[donor_id]))
})

# ==============================================================================
# Test 4: Incompatible blood types don't match
# ==============================================================================
test_that("R version - Incompatible types don't match", {
  cat("\n=== TEST 4: Incompatible blood types ===\n")

  data <- data.frame(
    id = 1:4,
    blood_type = c("A+", "B+", "A-", "B-")
  )

  donors <- c(1, 3)  # A+ and A-
  receivers <- c(2, 4)  # B+ and B-

  compatibility_table <- create_compatibility_table()

  result <- hopcroft_karp(donors, receivers, data, compatibility_table, can_receive)

  expect_equal(result$matching_size, 0)
  cat("✓ No matches found (as expected)\n")
})

# ==============================================================================
# Test 5: Large dataset performance test
# ==============================================================================
test_that("R version - Large dataset performance", {
  cat("\n=== TEST 5: Large dataset (100 individuals) ===\n")

  set.seed(123)
  data <- data.frame(
    id = 1:100,
    blood_type = sample(c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-"),
                        100, replace = TRUE,
                        prob=c(0.34, 0.06, 0.10, 0.02, 0.04, 0.01, 0.38, 0.05))
  )

  donors <- sample(data$id, 50)
  receivers <- setdiff(data$id, donors)

  compatibility_table <- create_compatibility_table()

  start_time <- Sys.time()
  result <- hopcroft_karp(donors, receivers, data, compatibility_table, can_receive)
  end_time <- Sys.time()

  execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  expect_gte(result$matching_size, 0)
  expect_lte(result$matching_size, 50)

  cat(sprintf("✓ Matching size: %d/%d\n", result$matching_size, length(donors)))
  cat(sprintf("✓ Execution time: %.3f seconds\n", execution_time))

  # Verify matching consistency
  for (donor in donors) {
    receiver <- result$matching_donor[[as.character(donor)]]
    if (!is.na(receiver)) {
      matched_donor <- result$matching_receiver[[as.character(receiver)]]
      expect_equal(matched_donor, donor)
    }
  }
  cat("✓ Matching consistency verified\n")
})

# ==============================================================================
# Test 6: Verify compatibility in matches
# ==============================================================================
test_that("R version - All matches respect compatibility", {
  cat("\n=== TEST 6: Compatibility verification ===\n")

  set.seed(456)
  data <- data.frame(
    id = 1:50,
    blood_type = sample(c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-"),
                        50, replace = TRUE,
                        prob=c(0.34, 0.06, 0.10, 0.02, 0.04, 0.01, 0.38, 0.05))
  )

  donors <- sample(data$id, 25)
  receivers <- setdiff(data$id, donors)

  compatibility_table <- create_compatibility_table()

  result <- hopcroft_karp(donors, receivers, data, compatibility_table, can_receive)

  # Verify each match is compatible
  matches_checked <- 0
  for (donor in donors) {
    receiver <- result$matching_donor[[as.character(donor)]]
    if (!is.na(receiver)) {
      donor_type <- data$blood_type[donor]
      receiver_type <- data$blood_type[receiver]

      is_compatible <- can_receive(donor_type, receiver_type, compatibility_table)
      expect_true(is_compatible,
                  info = sprintf("Donor %s -> Receiver %s should be compatible",
                                 donor_type, receiver_type))
      matches_checked <- matches_checked + 1
    }
  }

  cat(sprintf("✓ All %d matches verified as compatible\n", matches_checked))
})
