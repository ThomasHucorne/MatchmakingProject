#' Test the runtime of Gale-Shapley
#'
#' @description Runs the algorithm for random preference lists of size n and returns execution time.
#' @param n Number of men/women.
#' @return Execution time in seconds.
#' @export
test_gs_time <- function(n) {
  men <- paste0("M", 1:n)
  women <- paste0("W", 1:n)

  men_prefs <- lapply(1:n, function(i) sample(women))
  names(men_prefs) <- men

  women_prefs <- lapply(1:n, function(i) sample(men))
  names(women_prefs) <- women

  time <- system.time({
    gale_shapley(men_prefs, women_prefs)
  })
  return(time["elapsed"])
}
