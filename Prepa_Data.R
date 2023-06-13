library(readxl)
library(rlang)
library(dplyr)
library(xts)
library(lubridate)
library(stats)
library(forecast)
data <- read.csv("stat_acc_V3.csv", sep = ";")

# Enleve les cases vide
data <- na.omit(data) 

# Charger les données des arrondissements de Paris, Marseille et Lyon
arrondissements <- read.csv("donnees_arrondissements.csv", sep = ";")

# Jointure des données des accidents avec les données des arrondissements
data <- merge(data, arrondissements, by.x = "ville", by.y = "ville", all.x = TRUE)

# Créer une liste des villes à rechercher
villes <- c("PARIS", "MARSEILLE", "LYON")

# Remplacer les latitudes et longitudes des accidents dans Paris, Marseille et Lyon
for (ville in villes) {
  pattern <- paste0("^", ville)  # Construire le motif de recherche
  indices <- grepl(pattern, data$ville)  # Rechercher les indices correspondants
  data$latitude[indices] <- data$latitude_arr[indices]  # Remplacer les latitudes
  data$longitude[indices] <- data$longitude_arr[indices]  # Remplacer les longitudes
}

# Supprimer les colonnes supplémentaires des données des arrondissements
data <- subset(data, select = -c(latitude_arr, longitude_arr))

# Enleve les longitudes et latitude incohérentes
data <- subset(data, latitude >= -90 & latitude <= 90 & longitude >= -180 & longitude <= 180) 

# Enleve les lignes comportant une case NULL
data <- subset(data, !apply(data == 'NULL', 1, any))

# Répertorie et numérise plusieurs catégories 
data$descr_cat_veh <- as.numeric(factor(data$descr_cat_veh))

data <- data %>% mutate(descr_grav = case_when(
    descr_grav == "Indemne" ~ 1,
    descr_grav == "Blessé léger" ~ 2,
    descr_grav == "Blessé hospitalisé" ~ 3,
    descr_grav == "Tué" ~ 4
  ))

data <- data %>% mutate(descr_agglo = case_when(
  descr_agglo == "En agglomération" ~ 1,
  descr_agglo == "Hors agglomération" ~ 2
))

data <- data %>% mutate(descr_athmo = case_when(
  descr_athmo == "Autre" ~ 1,
  descr_athmo == "Brouillard – fumée" ~ 2,
  descr_athmo == "Neige – grêle" ~ 3,
  descr_athmo == "Normale" ~ 4,
  descr_athmo == "Pluie forte" ~ 5,
  descr_athmo == "Pluie légère" ~ 6,
  descr_athmo == "Temps éblouissant" ~ 7,
  descr_athmo == "Temps couvert" ~ 8,
  descr_athmo == "Vent fort – tempête" ~ 9
))

data <- data %>% mutate(descr_lum = case_when(
  descr_lum == "Crépuscule ou aube" ~ 1,
  descr_lum == "Nuit avec éclairage public allumé" ~ 2,
  descr_lum == "Nuit avec éclairage public non allumé" ~ 3,
  descr_lum == "Nuit sans éclairage public" ~ 4,
  descr_lum == "Plein jour" ~ 5
))

data <- data %>% mutate(descr_etat_surf = case_when(
  descr_etat_surf == "Autre" ~ 1,
  descr_etat_surf == "Boue" ~ 2,
  descr_etat_surf == "Corps gras – huile" ~ 3,
  descr_etat_surf == "Enneigée" ~ 4,
  descr_etat_surf == "Flaques" ~ 5,
  descr_etat_surf == "Inondée" ~ 6,
  descr_etat_surf == "Mouillée" ~ 7,
  descr_etat_surf == "Normale" ~ 8,
  descr_etat_surf == "Verglacée" ~ 9
))

data <- data %>% mutate(description_intersection = case_when(
  description_intersection == "Autre intersection" ~ 1,
  description_intersection == "Giratoire" ~ 2,
  description_intersection == "Hors intersection" ~ 3,
  description_intersection == "Intersection à plus de 4 branches" ~ 4,
  description_intersection == "Intersection en T" ~ 5,
  description_intersection == "Intersection en X" ~ 6,
  description_intersection == "Intersection en Y" ~ 7,
  description_intersection == "Passage à niveau" ~ 8,
  description_intersection == "Place" ~ 9
))

data <- data %>% mutate(descr_dispo_secu = case_when(
  descr_dispo_secu == "Autre - Non determinable" ~ 1,
  descr_dispo_secu == "Autre - Non utilisé" ~ 2,
  descr_dispo_secu == "Autre - Utilisé" ~ 3,
  descr_dispo_secu == "Présence équipement réfléchissant - Utilisation non déterminable" ~ 4,
  descr_dispo_secu == "Présence de ceinture de sécurité non utilisée" ~ 5,
  descr_dispo_secu == "Présence d'un dispositif enfant - Utilisation non déterminable" ~ 6,
  descr_dispo_secu == "Présence équipement réfléchissant non utilisé" ~ 7,
  descr_dispo_secu == "Présence d'un casque - Utilisation non déterminable" ~ 8,
  descr_dispo_secu == "Présence d'un casque non utilisé" ~ 9,
  descr_dispo_secu == "Présence d'un dispositif enfant non utilisé" ~ 10,
  descr_dispo_secu == "Présence d'une ceinture de sécurité - Utilisation non déterminable" ~ 11,
  descr_dispo_secu == "Utilisation d'un équipement réfléchissant" ~ 12,
  descr_dispo_secu == "Utilisation d'un casque" ~ 13,
  descr_dispo_secu == "Utilisation d'un dispositif enfant" ~ 14,
  descr_dispo_secu == "Utilisation d'une ceinture de sécurité" ~ 15,
))

data <- data %>% mutate(descr_motif_traj = case_when(
  descr_motif_traj == "Autre" ~ 1,
  descr_motif_traj == "Courses – achats" ~ 2,
  descr_motif_traj == "Domicile – école" ~ 3,
  descr_motif_traj == "Domicile – travail" ~ 4,
  descr_motif_traj == "Promenade – loisirs" ~ 5,
  descr_motif_traj == "Utilisation professionnelle" ~ 6
))

data <- data %>% mutate(descr_type_col = case_when(
  descr_type_col == "Autre collision" ~ 1,
  descr_type_col == "Deux véhicules - Frontale" ~ 2,
  descr_type_col == "Deux véhicules – Par l'arrière" ~ 3,
  descr_type_col == "Deux véhicules – Par le côté" ~ 4,
  descr_type_col == "Sans collision" ~ 5,
  descr_type_col == "Trois véhicules et plus – Collisions multiples" ~ 6,
  descr_type_col == "Trois véhicules et plus – En chaîne" ~ 7
))

# Mise des variables numériques sous format numériques, date sous format date, etc...

data$Num_Acc <- as.numeric(data$Num_Acc)
data$num_veh <- as.character(data$num_veh)
data$id_usa <- as.numeric(data$id_usa)
data$date <- as.POSIXct(data$date, format = "%Y-%m-%d %H:%M:%S")
data$ville <- as.character(data$ville)
data$id_code_insee <- as.character(data$id_code_insee)
data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)
data$an_nais <- as.numeric(data$an_nais)
data$age <- as.numeric(data$age)
data$place <- as.numeric(data$place)

# Série chronologique sur l’évolution du nombre d’accidents par mois
accidents_par_mois_counts <- table(format(data$date, "%Y-%m"))
plot(accidents_par_mois_counts, type = "l")
acf(accidents_par_mois_counts)

# Série chronologique sur l’évolution du nombre d’accidents par semaines
accidents_par_semaine_counts <- table(format(data$date, "%Y-%U"))
plot(accidents_par_semaine_counts, type = "l")
acf(accidents_par_semaine_counts)

# Lecture du fichier Excel
data_pop <- read_excel("base-cc-evol-struct-pop-2009.xls", range = "A6:E36686")

# Sélection des colonnes pertinentes : code INSEE, région et population
data_pop <- data_pop %>% select(`CODGEO`, `REG`, `P09_POP`)

# Agrégation des données par région pour calculer le nombre total d'habitants
habitants_par_region <- data_pop %>%
  group_by(`REG`) %>%
  summarise(Nombre_habitants = sum(`P09_POP`))

# Affichage du résultat
print(habitants_par_region, n = 50)

# Jointure des deux tables sur le code INSEE
data <- left_join(data, data_pop, by = c("id_code_insee" = "CODGEO"))

# Vérification de la nouvelle table
head(data)

# Agrégation des données pour calculer le nombre d'accidents par région et par gravité
accidents_par_region <- data %>%
  group_by(REG, descr_grav) %>%
  summarise(Nombre_accidents = n(), .groups = 'drop')

# Jointure avec le nombre d'habitants par région
accidents_par_region <- left_join(accidents_par_region, habitants_par_region, by = "REG")

# Calcul du nombre d'accidents pour 100 000 habitants
accidents_par_region <- accidents_par_region %>% 
  mutate(Accidents_per_100k = (Nombre_accidents / Nombre_habitants) * 100000)

accidents_par_region <- accidents_par_region %>% mutate(descr_grav = case_when(
  descr_grav == 1 ~ "Indemne",
  descr_grav == 2 ~ "Blessé léger",
  descr_grav == 3 ~ "Blessé hospitalisé",
  descr_grav == 4 ~ "Tué"
))

accidents_par_region <- accidents_par_region %>% mutate(REG = case_when(
  REG == "01" ~ "Guadeloupe",
  REG == "02" ~ "Martinique",
  REG == "03" ~ "Guyane Française",
  REG == "04" ~ "La Réunion",
  REG == "82" ~ "Rhône-Alpes",
  REG == "22" ~ "Picardie",
  REG == "83" ~ "Auvergne",
  REG == "93" ~ "Provence-Alpes-Côte d'Azur",
  REG == "21" ~ "Champagne-Ardenne",
  REG == "73" ~ "Midi-Pyrénées",
  REG == "91" ~ "Languedoc-Roussillon",
  REG == "25" ~ "Basse-Normandie",
  REG == "54" ~ "Poitou-Charentes",
  REG == "24" ~ "Centre",
  REG == "74" ~ "Limousin",
  REG == "94" ~ "Corse",
  REG == "26" ~ "Bourgogne",
  REG == "53" ~ "Bretagne",
  REG == "72" ~ "Aquitaine",
  REG == "43" ~ "Franche-Comté",
  REG == "23" ~ "Haute-Normandie",
  REG == "52" ~ "Pays de la Loire",
  REG == "41" ~ "Lorraine",
  REG == "31" ~ "Nord-Pas-de-Calais",
  REG == "42" ~ "Alsace",
  REG == "11" ~ "Île-de-France",
))


# Affichage du résultat
accidents_par_region