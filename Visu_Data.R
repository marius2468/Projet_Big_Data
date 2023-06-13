source("Prepa_Data.R")

# Création de l'histogramme du nombre d'accidents par conditions atmosphériques
histogramme_conditions_atmospheriques <- data %>%
  group_by(descr_athmo) %>%
  summarise(Nombre_accidents = n())

# Renommer les catégories des conditions atmosphériques
histogramme_conditions_atmospheriques <- histogramme_conditions_atmospheriques %>%
  mutate(descr_athmo = case_when(
    descr_athmo == 1 ~ "Autre",
    descr_athmo == 2 ~ "Brouillard – fumée",
    descr_athmo == 3 ~ "Neige – grêle",
    descr_athmo == 4 ~ "Normale",
    descr_athmo == 5 ~ "Pluie forte",
    descr_athmo == 6 ~ "Pluie légère",
    descr_athmo == 7 ~ "Temps éblouissant",
    descr_athmo == 8 ~ "Temps couvert",
    descr_athmo == 9 ~ "Vent fort – tempête"
  ))

# Tracer l'histogramme
barplot(histogramme_conditions_atmospheriques$Nombre_accidents,
        names.arg = histogramme_conditions_atmospheriques$descr_athmo,
        main = "Nombre d'accidents par conditions atmosphériques",
        col = "lightblue",
        border = "black",
        las = 2)


# Création de l'histogramme du nombre d'accidents par conditions de surface
histogramme_conditions_surface <- data %>%
  group_by(descr_etat_surf) %>%
  summarise(Nombre_accidents = n())

histogramme_conditions_surface <- histogramme_conditions_surface %>%
  mutate(descr_etat_surf = case_when(
    descr_etat_surf == 1 ~ "Autre",
    descr_etat_surf == 2 ~ "Boue",
    descr_etat_surf == 3 ~ "Corps gras – huile",
    descr_etat_surf == 4 ~ "Enneigée",
    descr_etat_surf == 5 ~ "Flaques",
    descr_etat_surf == 6 ~ "Inondée",
    descr_etat_surf == 7 ~ "Mouillée",
    descr_etat_surf == 8 ~ "Normale",
    descr_etat_surf == 9 ~ "Verglacée"
  ))

# Tracer l'histogramme
barplot(histogramme_conditions_surface$Nombre_accidents,
        names.arg = histogramme_conditions_surface$descr_etat_surf,
        main = "Nombre d'accidents par conditions atmosphériques",
        col = "lightblue",
        border = "black",
        las = 2)


# Création de l'histogramme du nombre d'accidents selon la gravité
histogramme_gravité <- data %>%
  group_by(descr_grav) %>%
  summarise(Nombre_accidents = n())

histogramme_gravité <- histogramme_gravité %>% mutate(descr_grav = case_when(
  descr_grav == 1 ~ "Indemne",
  descr_grav == 2 ~ "Blessé léger",
  descr_grav == 3 ~ "Blessé hospitalisé",
  descr_grav == 4 ~ "Tué"
))

# Tracer l'histogramme
barplot(histogramme_gravité$Nombre_accidents,
        names.arg = histogramme_gravité$descr_grav,
        main = "Nombre d'accidents selon la gravité",
        col = "lightblue",
        border = "black",
        las = 2)

 
# Création du tableau du nombre d'accidents par heure
accidents_par_heure <- table(format(data$date, "%H"))

# Conversion du tableau en dataframe
accidents_par_heure <- as.data.frame(accidents_par_heure)
names(accidents_par_heure) <- c("Heure", "Nombre_accidents")

# Tracer la courbe du nombre d'accidents par heure
plot(accidents_par_heure$Nombre_accidents, type = "l",
     xlab = "Heure",
     ylab = "Nombre d'accidents",
     main = "Nombre d'accidents par heure",
     xlim = c(0, 23),
     col = "blue", xaxt = "n")

# Ajouter des étiquettes sur l'axe des abscisses
axis(1, at = 0:23, labels = paste0(0:23, "h"))

# Ajouter une grille en arrière-plan
grid(lwd = 0.5)


# Calculer le nombre d'accidents par ville
accidents_par_ville <- table(data$ville)

# Trier les villes par nombre d'accidents décroissant
villes_tries <- sort(accidents_par_ville, decreasing = TRUE)

# Sélectionner les 20 villes ayant le plus d'accidents
top_20_villes <- villes_tries[1:20]

# Tracer l'histogramme du nombre d'accidents pour les 20 villes
barplot(top_20_villes, horiz = FALSE,
        ylab = "Nombre d'accidents",
        main = "Nombre d'accidents pour les 20 villes les plus touchées",
        col = "lightblue",
        border = "black",
        las = 2)




# write.csv(data, file = "export.csv", row.names = FALSE)