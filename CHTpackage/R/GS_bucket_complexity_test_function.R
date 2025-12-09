# Import fichier.R ?

#' Test the runtime of Gale-Shapley with bucket
#'
#' @description Runs the algorithm for random preference lists of size n and returns execution time.
#' @param n Number of men/women.
#' @return Execution time in seconds.
#' @export
test_gs_bucket_time <- function(n) {
  men <- paste0("M", 1:n)
  women <- paste0("W", 1:n)

  men_prefs <- lapply(1:n, function(i) sample(women))
  names(men_prefs) <- men

  women_prefs <- lapply(1:n, function(i) sample(men))
  names(women_prefs) <- women

  time <- system.time({
    best_gs_bucket(men_prefs, women_prefs) # CHANGEZ ICI L'ALGO QUE VOUS VOULEZ METTRE
  })
  return(time["elapsed"])
}
