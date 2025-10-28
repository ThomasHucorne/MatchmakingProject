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

best_gs_heap <- function(men_prefs, women_prefs){
  n <- length(men_prefs)
  next_women <- rep(1, n)
  matching <- rep(NA, n)

  # Inversion of feminine preferences to compare quicker
  rank_women <- lapply(1:n, function(f)
    order(match(1:n, women_prefs[[f]])))

  # Heap global : data.frame simulating a heap (sort on priority)
  heap <- data.frame(
    h = 1:n,
    f = sapply(1:n, function(i) men_prefs[[i]][1]),
    prio = 1
  )

  while (nrow(heap) > 0) {
    # Extract the suggestion that is the priority
    idx <- which.min(heap$prio)
    prop <- heap[idx,]; heap <- heap[-idx,]
    h <- prop$h
    f <- prop$f

    current <- which(matching == f)

    if (length(current) == 0) {
      matching[h] <- f
    } else {
      pire <- current[which.max(rank_women[[f]][current])]
      if (rank_women[[f]][h] < rank_women[[f]][pire]) {
        matching[h] <- f
        matching[pire] <- NA
        next_women[pire] <- next_women[pire] + 1
        if (next_women[pire] <= length(men_prefs[[pire]])) {
          f2 <- men_prefs[[pire]][next_women[pire]]
          heap <- rbind(heap, data.frame(h=pire, f=f2, prio=next_women[pire]))
        }
      } else {
        next_women[h] <- next_women[h] + 1
        if (next_women[h] <= length(men_prefs[[h]])) {
          f2 <- men_prefs[[h]][next_women[h]]
          heap <- rbind(heap, data.frame(h=h, f=f2, prio=next_women[h]))
        }
      }
    }
  }

  data.frame(Man = names(men_prefs), Woman = names(women_prefs)[matching])
}

