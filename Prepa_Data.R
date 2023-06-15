# Libraries
library(readxl)
library(rlang)
library(dplyr)
library(xts)
library(lubridate)
library(stats)
library(forecast)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap)
library(rgdal)
library(geojsonio)
library(mapview)
library(tibble)
library(foreign)
library(tidyverse)

# Read data
data <- read.csv("stat_acc_V3.csv", sep = ";")

# Remove NA values
data <- na.omit(data) 

# Load district data for Paris, Marseille, and Lyon
districtsData <- read.csv("donnees_arrondissements.csv", sep = ";")

# Join accident data with district data
data <- merge(data, districtsData, by.x = "ville", by.y = "ville", all.x = TRUE)

# Replace latitudes and longitudes of accidents in Paris, Marseille, and Lyon
for (city in c("PARIS", "MARSEILLE", "LYON")) {
  pattern <- paste0("^", city)  # Search pattern
  indices <- grepl(pattern, data$ville)  # Corresponding indices
  data$latitude[indices] <- data$latitudeArr[indices]  # Replace latitudes
  data$longitude[indices] <- data$longitudeArr[indices]  # Replace longitudes
}

# Remove additional columns of district data
data <- subset(data, select = -c(latitudeArr, longitudeArr))

# Remove data with incorrect latitude and longitude
data <- subset(data, latitude >= 40 & latitude <= 52 & longitude >= -6 & longitude <= 10) 

# Remove rows with NULL values
data <- subset(data, !apply(data == 'NULL', 1, any))

# Make sure 'an_nais' is numeric
data$an_nais <- as.numeric(data$an_nais)

# Calculate age based on current year
data <- data %>%
  mutate(age = as.integer(format(Sys.Date(), "%Y")) - an_nais)

# List and digitize several categories 
data$descr_cat_veh <- as.numeric(factor(data$descr_cat_veh))

data <- data %>% mutate(descr_grav = case_when(
  descr_grav == "Indemne" ~ 1,
  descr_grav == "Blessé léger" ~ 2,
  descr_grav == "Blessé hospitalisé" ~ 3,
  descr_grav == "Tué" ~ 4
))

data <- data %>% mutate(descr_agglo = case_when(
  descr_agglo =="Hors agglomération" ~ 1,
  descr_agglo == "En agglomération" ~ 2,
))

data <- data %>% mutate(descr_athmo = case_when(
  descr_athmo == "Non renseigné" ~ -1,
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
  descr_etat_surf == "Non renseigné" ~ -1,
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
  descr_dispo_secu == "Autre - Non déterminable" ~ 1,
  descr_dispo_secu == "Autre - Non utilisé" ~ 2,
  descr_dispo_secu == "Autre - Utilisé" ~ 3,
  descr_dispo_secu == "Présence d'un casque - Utilisation non déterminable" ~ 4,
  descr_dispo_secu == "Présence d'un casque non utilisé " ~ 5,
  descr_dispo_secu == "Présence d'un dispositif enfant non utilisé" ~ 6,
  descr_dispo_secu == "Présence d'un équipement réfléchissant non utilisé" ~ 7,
  descr_dispo_secu == "Présence d'une ceinture de sécurité - Utilisation non déterminable" ~ 8,
  descr_dispo_secu == "Présence de ceinture de sécurité non utilisée " ~ 9,
  descr_dispo_secu == "Présence dispositif enfant - Utilisation non déterminable" ~ 10,
  descr_dispo_secu == "Présence équipement réfléchissant - Utilisation non déterminable" ~ 11,
  descr_dispo_secu == "Utilisation d'un casque " ~ 12,
  descr_dispo_secu == "Utilisation d'un équipement réfléchissant " ~ 13,
  descr_dispo_secu == "Utilisation d'une ceinture de sécurité " ~ 14,
  descr_dispo_secu == "Utilisation d'un dispositif enfant" ~ 15
))

data <- data %>% mutate(descr_motif_traj = case_when(
  descr_motif_traj == "Non renseigné" ~ -1,
  descr_motif_traj == "Autre" ~ 1,
  descr_motif_traj == "Courses – achats" ~ 2,
  descr_motif_traj == "Domicile – école" ~ 3,
  descr_motif_traj == "Domicile – travail" ~ 4,
  descr_motif_traj == "Promenade – loisirs" ~ 5,
  descr_motif_traj == "Utilisation professionnelle" ~ 6
))

data <- data %>% mutate(descr_type_col = case_when(
  descr_type_col == "Non renseigné" ~ -1,
  descr_type_col == "Deux véhicules - Frontale" ~ 1,
  descr_type_col == "Deux véhicules – Par l’arrière" ~ 2,
  descr_type_col == "Deux véhicules – Par le coté" ~ 3,
  descr_type_col == "Trois véhicules et plus – En chaîne" ~ 4,
  descr_type_col == "Trois véhicules et plus – Collisions multiples" ~ 5,
  descr_type_col == "Sans collision" ~ 6,
  descr_type_col == "Autre collision" ~ 7
))



# Convert variables into their appropriate types
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

# Time series on the evolution of the number of accidents per month
accidentsPerMonth <- table(format(data$date, "%Y-%m"))
plot(accidentsPerMonth, type = "l")
acf(accidentsPerMonth)

# Time series on the evolution of the number of accidents per week
accidentsPerWeek <- table(format(data$date, "%Y-%U"))
plot(accidentsPerWeek, type = "l")
acf(accidentsPerWeek)

# Read Excel file
popData <- read_excel("base-cc-evol-struct-pop-2009.xls", range = "A6:E36686")

# Select relevant columns: INSEE code, region, and population
popData <- popData %>% select(CODGEO, REG, P09_POP)

# Aggregate data by region to calculate total number of inhabitants
inhabitantsPerRegion <- popData %>%
  group_by(REG) %>%
  summarise(totalInhabitants = sum(P09_POP))

# Join the two tables on the INSEE code
data <- left_join(data, popData, by = c("id_code_insee" = "CODGEO"))

# Aggregate data to calculate the number of accidents by region and severity
accidentsPerRegion <- data %>%
  group_by(REG, descr_grav) %>%
  summarise(totalAccidents = n(), .groups = 'drop')

# Join with the number of inhabitants per region
accidentsPerRegion <- left_join(accidentsPerRegion, inhabitantsPerRegion, by = "REG")

# Calculation of the number of accidents per 100,000 inhabitants
accidentsPerRegion <- accidentsPerRegion %>% 
  mutate(accidentsPer100k = (totalAccidents / totalInhabitants) * 100000)

# Renaming severity level based on the provided key-value pair
accidentsPerRegion <- accidentsPerRegion %>% mutate(descr_grav = case_when(
  descr_grav == 1 ~ "Indemne",
  descr_grav == 2 ~ "Blessé léger",
  descr_grav == 3 ~ "Blessé hospitalisé",
  descr_grav == 4 ~ "Tué"
))

# Renaming regions based on the provided key-value pair
accidentsPerRegion <- accidentsPerRegion %>% mutate(REG = case_when(
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

# Displaying the final data
print(accidentsPerRegion)
