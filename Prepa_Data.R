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

data$descr_agglo <- as.numeric(factor(data$descr_agglo))
data$descr_athmo <- as.numeric(factor(data$descr_athmo))
data$descr_lum <- as.numeric(factor(data$descr_lum))
data$descr_etat_surf <- as.numeric(factor(data$descr_etat_surf))
data$description_intersection <- as.numeric(factor(data$description_intersection))
data$descr_dispo_secu <- as.numeric(factor(data$descr_dispo_secu))
data$descr_motif_traj <- as.numeric(factor(data$descr_motif_traj))
data$descr_type_col <- as.numeric(factor(data$descr_type_col))

# Mise des variables numériques sous format numériques, date sous format date, etc...

data$Num_Acc <- as.numeric(data$Num_Acc)
data$num_veh <- as.character(data$num_veh)
data$id_usa <- as.numeric(data$id_usa)
data$date <- as.Date(data$date)
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
print(accidents_par_region, n=100)


