source("Prepa_Data.R")


# Création de l'histogramme du nombre d'accidents par conditions atmosphériques
histogramme_conditions_atmospheriques <- data %>%
  group_by(descr_athmo) %>%
  summarise(Nombre_accidents = n())

# Renommer les catégories des conditions atmosphériques
histogramme_conditions_atmospheriques <- histogramme_conditions_atmospheriques %>%
  mutate(descr_athmo = case_when(
    descr_athmo == -1 ~ "Non renseigné",
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



# Création de l'histogramme du nombre d'accidents par tranche d'âge

histogramme_age <- data %>% mutate(age = case_when(
  age < 18 ~ "Moins de 18 ans",
  age >= 18 & age <= 24 ~ "18-24 ans",
  age >= 25 & age <= 34 ~ "25-34 ans",
  age >= 35 & age <= 44 ~ "35-44 ans",
  age >= 45 & age <= 54 ~ "45-54 ans",
  age >= 55 & age <= 64 ~ "55-64 ans",
  age >= 65 ~ "65 ans et plus"
))

histogramme_age <- histogramme_age %>%
  group_by(age) %>%
  summarise(Nombre_accidents = n())


# Tracer l'histogramme
barplot(histogramme_age$Nombre_accidents,
        names.arg = histogramme_age$age,
        main = "Nombre d'accidents par tranche d'âge",
        col = "lightblue",
        border = "black",
        las = 2)


# Création d'une nouvelle colonne 'mois' pour extraire le mois à partir de la colonne 'date'
data$mois <- format(data$date, "%Y-%m")

# Calculer la moyenne mensuelle des accidents
moyenne_mensuelle <- data %>%
  group_by(mois) %>%
  summarise(moyenne_accidents = mean(n()))


# Tracer le graphique de la moyenne mensuelle des accidents
barplot(moyenne_mensuelle$moyenne_accidents,
        names.arg = moyenne_mensuelle$mois,
        main = "Moyenne mensuelle des accidents",
        col = "lightblue",
        border = "black",
        las = 2)


# Charger les données géographiques de la France
carte <- st_read("regions.geojson")

region_info <- read.csv("anciennes-nouvelles-regions.csv", sep = ";", stringsAsFactors = FALSE)

# Renommer les colonnes pour une meilleure clarté
colnames(region_info) <- c("Nouveau_Code", "Nouveau_Nom", "Anciens_Code", "Anciens_Nom")

# Fusionner le dataframe data avec region_info en utilisant la colonne "REG" dans data
# et la colonne "Anciens_Code" dans region_info
new_data <- merge(data, region_info, by.x = "REG", by.y = "Anciens_Code")
# Convertir la variable data en un objet spatial

nb_reg <- new_data %>%
  group_by(Nouveau_Code) %>%
  summarise(Nombre_accidents = n())


nb_reg$Nouveau_Code <- as.character(nb_reg$Nouveau_Code)

carte_joined <- inner_join(carte, nb_reg, by = c("code" = "Nouveau_Code"))

# affichage de la carte
mapview(carte_joined)


carte2 <- st_read("departements.geojson")


data$dep <- ifelse(nchar(data$id_code_insee) == 5, 
                             substr(data$id_code_insee, 1, 2), 
                             paste0("0", substr(data$id_code_insee, 1, 1)))

print(carte2$code)

print(data)

nb_dep <- data %>%
  group_by(dep) %>%
  summarise(Nombre_accidents = n())

nb_dep$dep <- as.character(nb_dep$dep)

carte2_joined <- inner_join(carte2, nb_dep, by = c("code" = "dep"))

mapview(carte2_joined)

carte3 <- st_read("regions.geojson")

accidents_graves_par_region <- new_data %>%
  filter(descr_grav %in% c(3, 4)) %>%
  group_by(Nouveau_Code) %>%
  summarise(Nombre_accidents_graves = n())

print(accidents_graves_par_region)

accidents_par_region <- new_data %>%
  group_by(Nouveau_Code) %>%
  summarise(Nombre_accidents = n())

print(accidents_par_region)


# Jointure avec le nombre d'habitants par région
accidents_grave_par_region <- left_join(accidents_graves_par_region, accidents_par_region, by = "Nouveau_Code")

taux_accidents_grave_par_region <- accidents_grave_par_region %>%
  mutate(Taux_accidents_graves = (Nombre_accidents_graves / Nombre_accidents) * 100)

taux_accidents_grave_par_region$Nouveau_Code <- as.character(taux_accidents_grave_par_region$Nouveau_Code)

taux_accidents_grave_par_region <- taux_accidents_grave_par_region %>%
  mutate(Taux_accidents_graves = paste0(round(Taux_accidents_graves, 2), "%"))


print(taux_accidents_grave_par_region)

carte3_joined <- inner_join(carte, taux_accidents_grave_par_region, by = c("code" = "Nouveau_Code"))


mapview(carte3_joined)

# write.csv(data, file = "export.csv", row.names = FALSE)