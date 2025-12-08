#' Gale-Shapley Stable Matching Algorithm
#'
#' @description Implements the Gale-Shapley algorithm for stable matching.
#' @param men_prefs A list of men's preferences (each element is a vector of women names).
#' @param women_prefs A list of women's preferences (each element is a vector of men names).
#' @return A data.frame with columns "Man" and "Woman" representing the stable matches.
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
#' gale_shapley(men_prefs, women_prefs)
#' @export
gale_shapley <- function(men_prefs, women_prefs) {
  free_men <- names(men_prefs)
  engaged <- list()
  next_proposal <- setNames(rep(1, length(free_men)), free_men)

  # Helper function: rank of a man for a woman
  rank <- lapply(women_prefs, function(prefs) setNames(seq_along(prefs), prefs))

  while (length(free_men) > 0) {
    man <- free_men[1]
    woman <- men_prefs[[man]][next_proposal[man]]

    # Man proposes
    if (is.null(engaged[[woman]])) {
      engaged[[woman]] <- man
      free_men <- setdiff(free_men, man)
    } else {
      current <- engaged[[woman]]
      if (rank[[woman]][[man]] < rank[[woman]][[current]]) {
        engaged[[woman]] <- man
        free_men <- c(free_men, current)
        free_men <- setdiff(free_men, man)
      } else {
        # Woman rejects this man
      }
    }

    next_proposal[man] <- next_proposal[man] + 1
  }

  matches <- data.frame(
    Man = unlist(engaged),
    Woman = names(engaged),
    stringsAsFactors = FALSE
  )
  return(matches)
}
