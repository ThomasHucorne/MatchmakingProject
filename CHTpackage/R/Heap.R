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

best_gs_bucket <- function(men_prefs, women_prefs) {
  # Convert names to numeric indices for manipulation
  men_names   <- names(men_prefs)
  women_names <- names(women_prefs)

  n <- length(men_prefs)

  men_index   <- setNames(seq_along(men_names), men_names)
  women_index <- setNames(seq_along(women_names), women_names)

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

  # Bucket queue
  buckets <- vector("list", n)

  # Initialize with priority = 1
  for (h in seq_len(n)) {
    f <- men_pref_num[[h]][1]
    buckets[[1]] <- append(buckets[[1]], list(c(h,f)))
  }

  # While there is at least one non-empty bucket
  while (any(lengths(buckets) > 0)) {

    # Find smallest priority bucket that has proposals
    p <- match(TRUE, lengths(buckets) > 0)

    # Pop one proposal
    prop <- buckets[[p]][[1]]
    buckets[[p]] <- buckets[[p]][-1]

    h <- prop[1]
    f <- prop[2]

    # Find current fiancé of f
    current <- match(f, matching)

    if (is.na(current)) {

      matching[h] <- f

    } else {

      rank_current <- women_rank[[f]][as.character(current)]
      rank_new     <- women_rank[[f]][as.character(h)]

      if (rank_new < rank_current) {
        # Woman prefers new man
        matching[h] <- f
        matching[current] <- NA

        next_choice[current] <- next_choice[current] + 1L
        if (next_choice[current] <= length(men_pref_num[[current]])) {
          f2 <- men_pref_num[[current]][next_choice[current]]
          buckets[[next_choice[current]]] <- append(buckets[[next_choice[current]]], list(c(current,f2)))
        }

      } else {
        # Man rejected -> propose to next woman
        next_choice[h] <- next_choice[h] + 1L
        if (next_choice[h] <= length(men_pref_num[[h]])) {
          f2 <- men_pref_num[[h]][next_choice[h]]
          buckets[[next_choice[h]]] <- append(buckets[[next_choice[h]]], list(c(h,f2)))
        }
      }
    }
  }

  data.frame(
    Man = men_names,
    Woman = women_names[matching],
    stringsAsFactors = FALSE
  )
}

