# Ici la dernière méthode, utilisant des données pratiques

# Exemple d'utilisation avec des données simulées
set.seed(123)
data <- data.frame(
  id = 1:50,
  blood_type = sample(c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-"), 100, replace = TRUE,
                      prob=c(0.34, 0.06, 0.10, 0.02, 0.04, 0.01, 0.38, 0.05))
)

# table des transfusions possibles entre groupes sanguins avec une matrice de compatibilité
compatibility_table <- matrix(
  c(1, 1, 1, 1, 0, 0, 0, 0,
    0, 1, 0, 1, 0, 0, 0, 0,
    0, 0, 1, 1, 0, 0, 0, 0,
    0, 0, 0, 1, 0, 0, 0, 0,
    0, 0, 1, 1, 1, 1, 0, 0,
    0, 0, 0, 1, 0, 1, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1,
    0, 1, 0, 1, 0, 1, 0, 1),
  nrow = 8,
  byrow = TRUE,
  dimnames = list(
    c("A+", "A-", "O+", "O-", "B+", "B-", "AB+", "AB-"),
    c("A+", "A-", "O+", "O-", "B+", "B-", "AB+", "AB-")
  )
)

can_receive <- function(donor_type, recipient_type, compatibility_table) {
  return(compatibility_table[donor_type, recipient_type] == 1)
}

# Fonction pour générer les préférences basées sur la compatibilité
generate_preferences <- function(data, compatibility_table) {
  donors <- split(data$id, data$blood_type)
  recipients <- split(data$id, data$blood_type)
  return (list(donors = donors, recipients = recipients))
}

generate_receivers_preferences_type <- function(compatibility_table) {
  preferences <- list()
  blood_types <- rownames(compatibility_table)
  for (donor_type in blood_types) {
    compatible_recipients <- blood_types[which(compatibility_table[donor_type, ] == 1)]
    preferences[[donor_type]] <- compatible_recipients
  }
  return(preferences)
}

generate_donors_preferences_type <- function(compatibility_table) {
  preferences <- list()
  blood_types <- rownames(compatibility_table)
  for (recipient_type in blood_types) {
    compatible_donors <- blood_types[which(compatibility_table[, recipient_type] == 1)]
    preferences[[recipient_type]] <- compatible_donors
  }
  return(preferences)
}

# example
donor_preferences_type <- generate_donors_preferences_type(compatibility_table)
receiver_preferences_type <- generate_receivers_preferences_type(compatibility_table)



