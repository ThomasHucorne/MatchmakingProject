#' Holpcroft-Karp algorithm for Blood Transfusion Matching
#' The last method using practical values
#'
#' @description description
#' #' This script implements the Hopcroft-Karp algorithm to find the maximum matching
#' #' between blood donors and recipients based on blood type compatibility.
#' #' It constructs a bipartite graph where edges represent compatible donor-recipient pairs
#' #' and uses BFS and DFS to find augmenting paths to maximize the number of successful
#' #' transfusions.
#'
#' @param donors A vector of donor IDs.
#' @param receivers A vector of receiver IDs.
#' @param data A data frame containing donor and receiver information, including blood types.
#' @param compatibility_table A matrix defining blood type compatibility.
#'
#' @return A list containing the matching results, including matched pairs and statistics.
#'
#' @examples
#' # Example usage with synthetic data
#' set.seed(123)
#' data <- data.frame(
#'   id = 1:100,
#'   blood_type = sample(c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-"), 100, replace = TRUE,
#'                       prob=c(0.34, 0.06, 0.10, 0.02, 0.04, 0.01, 0.38, 0.05))
#' )
#' donors <- sample(data$id, 50)
#' receivers <- setdiff(data$id, donors)
#' compatibility_table <- create_compatibility_table()
#'
#' result <- hopcroft_karp(donors, receivers, data, compatibility_table, can_receive)
#' print_matching_results(result, donors, receivers, data)
#'
#' @export


# ======================== Compatibility function =============================
create_compatibility_table <- function() {
  compatibility_table <- matrix(
    c(
      # Receveurs: A+   A-   B+   B-   AB+  AB-  O+   O-
      # A+ peut donner à :
      1,   0,   0,   0,   1,   0,   0,   0,  # Donneur A+
      # A- peut donner à :
      1,   1,   0,   0,   1,   1,   0,   0,  # Donneur A-
      # B+ peut donner à :
      0,   0,   1,   0,   1,   0,   0,   0,  # Donneur B+
      # B- peut donner à :
      0,   0,   1,   1,   1,   1,   0,   0,  # Donneur B-
      # AB+ peut donner à :
      0,   0,   0,   0,   1,   0,   0,   0,  # Donneur AB+
      # AB- peut donner à :
      0,   0,   0,   0,   1,   1,   0,   0,  # Donneur AB-
      # O+ peut donner à :
      1,   0,   1,   0,   1,   0,   1,   0,  # Donneur O+
      # O- peut donner à : (DONNEUR UNIVERSEL)
      1,   1,   1,   1,   1,   1,   1,   1   # Donneur O-
    ),
    nrow = 8,
    byrow = TRUE,
    dimnames = list(
      Donneurs = c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-"),
      Receveurs = c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-")
    )
  )

  return(compatibility_table)
}

can_receive <- function(donor_type, recipient_type, compatibility_table) {
  return(compatibility_table[donor_type, recipient_type] == 1)
}

# ========  Building the compatibility graph ===========================
build_compatibility_graph <- function(donors, receivers, data, compatibility_table, can_receive) {

  graph <- list()

  for (donor_id in donors) {
    donor_type <- data$blood_type[donor_id]
    compatible_receivers <- c()

    for (receiver_id in receivers) {
      receiver_type <- data$blood_type[receiver_id]

      if (can_receive(donor_type, receiver_type, compatibility_table)) {
        compatible_receivers <- c(compatible_receivers, receiver_id)
      }
    }

    graph[[as.character(donor_id)]] <- compatible_receivers
  }

  return(graph)
}


# ============ BFS : Find augmenting paths of minimal length ====
bfs_hopcroft_karp <- function(graph, donors, matching_donor, matching_receiver) {
  # Length initialization
  distance <- list()
  queue <- c()

  # Add all free donors to the queue
  for (donor in donors) {
    if (is.na(matching_donor[[as.character(donor)]])) {
      distance[[as.character(donor)]] <- 0 # ??
      queue <- c(queue, donor)
    } else {
      distance[[as.character(donor)]] <- Inf # ??
    }
  }

  # Special NIL node
  distance[["NIL"]] <- Inf

  # BFS
  repeat{
    donor <- queue[1] # is this enqueue operation?
    queue <- queue[-1]

    if (length(queue) == 0) {
      break
    }

    if (distance[[as.character(donor)]] < distance[["NIL"]]) { # should be because set to 0 if free and in queue
      # Explore every adjacent receiver
      adjacent_receivers <- graph[[as.character(donor)]]

      if (length(adjacent_receivers) > 0) {
        for (receiver in adjacent_receivers) {
          receiver_key <- as.character(receiver)

          # Found the donor paired to this receiver
          paired_donor <- matching_receiver[[receiver_key]]
          if (is.na(paired_donor)) {
            paired_donor_key <- "NIL"
          } else {
            paired_donor_key <- as.character(paired_donor)
          }

          # If this donnor has not been visited
          if (is.infinite(distance[[paired_donor_key]])) {
            distance[[paired_donor_key]] <- distance[[as.character(donor)]] + 1

            if (paired_donor_key != "NIL") {
              queue <- c(queue, paired_donor)
            }
          }
        }
      }
    }
  }

  # Return TRUE sif a path to a receiver exists
  return(list(found = !is.infinite(distance[["NIL"]]), distance = distance))
}


# ==============  DFS : Find and apply this augmenting path ==================
# Returns updated matching if successful
dfs_hopcroft_karp <- function(donor, graph, distance, matching_donor, matching_receiver) {
  if (!is.na(donor)) {
    adjacent_receivers <- graph[[as.character(donor)]]

    if (length(adjacent_receivers) > 0) {
      for (receiver in adjacent_receivers) {
        receiver_key <- as.character(receiver)

        # Found the donor paired to this receiver
        paired_donor <- matching_receiver[[receiver_key]]
        if (is.na(paired_donor)) {
          paired_donor_key <- "NIL"
        } else {
          paired_donor_key <- as.character(paired_donor)
        }

        # Verify the distance condition
        if (distance[[paired_donor_key]] == distance[[as.character(donor)]] + 1) {
          # Recursively try to find an augmenting path from the paired donor
          dfs_result <- dfs_hopcroft_karp(paired_donor, graph, distance, matching_donor, matching_receiver)

          if (dfs_result$success) {
            # Create the new matching
            dfs_result$matching_receiver[[receiver_key]] <- donor
            dfs_result$matching_donor[[as.character(donor)]] <- receiver
            return(dfs_result)
          } else {
            # Update the matchings even if not successful
            matching_donor <- dfs_result$matching_donor
            matching_receiver <- dfs_result$matching_receiver
            distance <- dfs_result$distance
          }
        }
      }
    }

    # Mark this donor as visited
    distance[[as.character(donor)]] <- Inf
    return(list(success = FALSE, matching_donor = matching_donor,
                matching_receiver = matching_receiver, distance = distance))
  }

  # Base case: reached NIL
  return(list(success = TRUE, matching_donor = matching_donor,
              matching_receiver = matching_receiver, distance = distance))
}

# ======================== Algorithm of Hopcroft-Karp =======================
hopcroft_karp <- function(donors, receivers, data, compatibility_table, can_receive) {
  # cat("Building compatibility graph...\n")
  graph <- build_compatibility_graph(donors, receivers, data, compatibility_table, can_receive)

  # Initialisation des structures de matching
  matching_donor <- list()
  matching_receiver <- list()

  for (donor in donors) {
    matching_donor[[as.character(donor)]] <- NA
  }

  for (receiver in receivers) {
    matching_receiver[[as.character(receiver)]] <- NA
  }

  matching_size <- 0
  iteration <- 0

  # cat("Research of maximum matching...\n")

  # While there exists an augmenting path
  while (iteration < 1000) {
    iteration <- iteration + 1
    bfs_result <- bfs_hopcroft_karp(graph, donors, matching_donor, matching_receiver)

    if (!bfs_result$found) {
      break
    }

    # For each free donor, try to find an augmenting path
    for (donor in donors) {
      if (is.na(matching_donor[[as.character(donor)]])) {
        dfs_result <- dfs_hopcroft_karp(donor, graph, bfs_result$distance,
                                        matching_donor, matching_receiver)

        if (dfs_result$success) {
          matching_size <- matching_size + 1
          # Get the updated matchings
          matching_donor <- dfs_result$matching_donor
          matching_receiver <- dfs_result$matching_receiver
        }
      }
    }

    # cat(sprintf("  Iteration %d : %d formed pairs\n", iteration, matching_size))
  }

  # cat(sprintf("\nMatching ended in %d iterations\n", iteration))

  # Build result
  result <- list(
    matching_donor = matching_donor,
    matching_receiver = matching_receiver,
    matching_size = matching_size,
    graph = graph
  )

  return(result)
}

# Function to print matching results
print_matching_results <- function(result, donors, receivers, data) {
  cat("\n========================================\n")
  cat("MATCHING RESULTS\n")
  cat("========================================\n\n")

  cat(sprintf("Total numbers of formed pairs : %d / %d\n",
              result$matching_size, length(donors)))
  cat(sprintf("Satisfaction rate : %.1f%%\n\n",
              (result$matching_size / length(donors)) * 100))

  # Print formed pairs
  cat("Formed pairs :\n")
  cat("----------------\n")
  pairs <- data.frame(
    Donneur_ID = integer(),
    Donneur_Type = character(),
    Receveur_ID = integer(),
    Receveur_Type = character(),
    stringsAsFactors = FALSE
  )

  for (donor in donors) {
    receiver <- result$matching_donor[[as.character(donor)]]
    if (!is.na(receiver)) {
      pairs <- rbind(pairs, data.frame(
        Donneur_ID = donor,
        Donneur_Type = data$blood_type[donor],
        Receveur_ID = receiver,
        Receveur_Type = data$blood_type[receiver],
        stringsAsFactors = FALSE
      ))
    }
  }

  if (nrow(pairs) > 0) {
    print(pairs, row.names = FALSE)
  }

  # Print unmatched donors
  cat("\n\nUnpaired donnors :\n")
  cat("------------------------\n")
  unmatched_donors <- c()
  for (donor in donors) {
    if (is.na(result$matching_donor[[as.character(donor)]])) {
      unmatched_donors <- c(unmatched_donors,
                            sprintf("D%d (%s)", donor, data$blood_type[donor]))
    }
  }

  if (length(unmatched_donors) > 0) {
    cat(paste(unmatched_donors, collapse = ", "), "\n")
  } else {
    cat("None\n")
  }

  # Print unmatched receivers
  cat("\nUnsatisfied receivers :\n")
  cat("---------------------------\n")
  unmatched_receivers <- c()
  for (receiver in receivers) {
    if (is.na(result$matching_receiver[[as.character(receiver)]])) {
      unmatched_receivers <- c(unmatched_receivers,
                               sprintf("R%d (%s)", receiver, data$blood_type[receiver]))
    }
  }

  if (length(unmatched_receivers) > 0) {
    cat(paste(unmatched_receivers, collapse = ", "), "\n")
  } else {
    cat("None\n")
  }

  cat("\n========================================\n")
}

