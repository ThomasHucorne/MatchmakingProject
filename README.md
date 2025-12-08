# Matchmaking Project

# Livrable
Rendre un Rmd, compilable en pdf ou html avec la méthode:
```
if (!requireNamespace("CHTpackage", quietly = TRUE)) {
if (!requireNamespace("remotes", quietly = TRUE)) {
install.packages("remotes")
}
remotes::install_github("ThomasHucorne/MatchmakingProject")
}

# Charge le package
library(CHTpackage)
```
Il faut avoir le package déposé sur GITHUB ! (ou autre)

## Description Problème & Etapes projet
Matchmaking Stable Matching
Avec 3 méthodes
(i) Le cas greedy O(n^2), explication de  l'approche avec préférence "homme ou femme".
(ii) Le cas d'utilisation d'un bucket dans ce problème. Quel tyde de structure c'est, comment ça réduit la complexité?
(iii) Exemple en santé, type hôpitaux en essayant d'être le plus réaliste possible. Quelles contraintes à rajouter / considérer pour le problème réel.

Les algorithmes du i) et du ii) seront en R et en Rcpp pour le ii) et iii).

Evaluation la complexité des algorithmes
Différence entre R et C++ (ordre de grandeur entre 50 et 100)
Différence entre les 3 algorithmes
