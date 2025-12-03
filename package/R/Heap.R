#' Best-First Gale-Shapley Stable Matching Algorithm with Heap
#'
#' @description
#' Implements a heap-based, best-first variant of the Gale-Shapley algorithm
#' for computing a stable matching between two sets: men and women.
#' Unlike the classic Gale-Shapley, this version uses a priority queue (heap)
#' to process proposals in order of preference, improving efficiency and
#' allowing better control over the order in which matches are considered.
#'
#' @param men_prefs A named list of men's preferences. Each element is a character
#'   vector of women names ordered from most to least preferred.
#' @param women_prefs A named list of women's preferences. Each element is a character
#'   vector of men names ordered from most to least preferred.
#'
#' @return A \code{data.frame} with columns "Man" and "Woman" representing the stable matches.
#'
#' @examples
#' men_prefs <- list(
#'   A = c("Z", "X", "Y"),
#'   B = c("Y", "X", "Z"),
#'   C = c("X", "Z", "Y")
#' )
#' women_prefs <- list(
#'   X = c("B", "A", "C"),
#'   Y = c("A", "B", "C"),
#'   Z = c("A", "C", "B")
#' )
#' best_gs_heap(men_prefs, women_prefs)
#'
#' @seealso
#' \link{gale_shapley} for the classic iterative Gale-Shapley algorithm.
#'
#' @export

best_gs_heap <- function(men_prefs, women_prefs) {
  # Convert names to numeric indices for manipulation
  men_names   <- names(men_prefs)
  women_names <- names(women_prefs)

  men_index   <- setNames(seq_along(men_names), men_names)
  women_index <- setNames(seq_along(women_names), women_names)

  n <- length(men_prefs)

  # Convert preference lists to numeric indices
  men_pref_num <- lapply(men_prefs, function(v) women_index[v])
  women_pref_num <- lapply(women_prefs, function(v) men_index[v])

  # Inverted preferences for women (ranking tables)
  women_rank <- lapply(women_pref_num, function(v) {
    ranks <- seq_along(v)
    names(ranks) <- v
    ranks
  })

  # Track: next woman each man will propose to
  next_choice <- rep(1, n)

  # Matching: matching[h] = woman index
  matching <- rep(NA, n)

  # Priority queue simulated by a data.frame
  heap <- data.frame(
    man   = seq_len(n),
    woman = sapply(seq_len(n), function(i) men_pref_num[[i]][1]),
    prio  = 1
  )

  while (nrow(heap) > 0) {
    # Extract min priority
    idx <- which.min(heap$prio)
    prop <- heap[idx, ]
    # Deleting the proposition of matching from heap
    heap <- heap[-idx, ]

    h <- prop$man      # man index
    f <- prop$woman    # woman index

    # Current fiancé of woman f
    current <- which(matching == f)

    if (length(current) == 0) {
      # Free → accept
      matching[h] <- f
    } else {
      # Woman compares preferences
      current <- current[1]
      rank_current <- women_rank[[f]][ as.character(current) ]
      rank_new     <- women_rank[[f]][ as.character(h) ]

      if (rank_new < rank_current) {
        # Replace fiancé
        matching[h] <- f
        matching[current] <- NA

        next_choice[current] <- next_choice[current] + 1
        if (next_choice[current] <= length(men_pref_num[[current]])) {
          f2 <- men_pref_num[[current]][ next_choice[current] ]
          heap <- rbind(heap, data.frame(man=current, woman=f2, prio=next_choice[current]))
        }
      } else {
        # Rejected → propose to next woman
        next_choice[h] <- next_choice[h] + 1
        if (next_choice[h] <= length(men_pref_num[[h]])) {
          f2 <- men_pref_num[[h]][ next_choice[h] ]
          heap <- rbind(heap, data.frame(man=h, woman=f2, prio=next_choice[h]))
        }
      }
    }
  }

  # Convert numeric matching back to names
  data.frame(
    Man   = men_names,
    Woman = women_names[ matching ],
    stringsAsFactors = FALSE
  )
}


