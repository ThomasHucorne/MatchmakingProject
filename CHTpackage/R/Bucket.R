#' Best-First Gale-Shapley Stable Matching Algorithm with bucket
#'
#' @description
#' Implements a bucket-based, best-first variant of the Gale-Shapley algorithm
#' for computing a stable matching between two sets: men and women.
#' Unlike the classic Gale-Shapley, this version uses a priority vector/list (bucket)
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
#' best_gs_bucket(men_prefs, women_prefs)
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
  men_pref_num <- vector("list", n)
  for (h in seq_len(n)) {
    men_pref_num[[h]] <- women_index[ men_prefs[[h]] ]
  }
  women_pref_num <- vector("list", n)
  for (f in seq_len(n)) {
    women_pref_num[[f]] <- men_index[ women_prefs[[f]] ]
  }
  # Inverted preferences for women (ranking tables)
  women_rank <- vector("list", n)
  for (f in seq_len(n)) {
    pref <- women_pref_num[[f]]
    r <- integer(n)
    for (pos in seq_len(n)) {
      r[ pref[pos] ] <- pos
    }
    women_rank[[f]] <- r
  }
  # Track: next woman each man will propose to
  next_choice <- rep(1L, n)
  # Matching: matching[h] = woman index
  matching    <- rep(NA_integer_, n)

  # buckets[[k]] contient des couples (h,f) où k = priorité = next_choice[h]
  buckets <- vector("list", n)

  # Initialize with priority = 1
  for (h in seq_len(n)) {
    f <- men_pref_num[[h]][1]
    buckets[[1]] <- append(buckets[[1]], list(c(h, f)))
  }
  # While there is at least one non-empty bucket
    while (TRUE) {
    # Find smallest priority bucket that has proposals
    p <- match(TRUE, lengths(buckets) > 0)
    if (is.na(p)) break
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
      rank_current <- women_rank[[f]][current]
      rank_new <- women_rank[[f]][h]

      if (rank_new < rank_current) {
        # Woman prefers new man
        matching[h]      <- f
        matching[current] <- NA_integer_

        next_choice[current] <- next_choice[current] + 1L
        if (next_choice[current] <= n) {
          f2 <- men_pref_num[[current]][next_choice[current]]
          buckets[[next_choice[current]]] <- append(buckets[[next_choice[current]]], list(c(current, f2)))
        }

      } else {
        # Man rejected -> propose to next woman
        next_choice[h] <- next_choice[h] + 1L
        if (next_choice[h] <= n) {
          f2 <- men_pref_num[[h]][next_choice[h]]
          buckets[[next_choice[h]]] <- append(buckets[[next_choice[h]]], list(c(h, f2)))
        }
      }
    }
  }
  data.frame(
    Man   = men_names,
    Woman = women_names[matching],
    stringsAsFactors = FALSE
  )
}
