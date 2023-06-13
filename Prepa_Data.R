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

<<<<<<< HEAD

# Compter le nombre d'accidents par semaine
=======
# Série chronologique sur l’évolution du nombre d’accidents par semaines
>>>>>>> ad2ec340e1213262c115e36864550f1afb2eb652
accidents_par_semaine_counts <- table(format(data$date, "%Y-%U"))
plot(accidents_par_semaine_counts, type = "l")
acf(accidents_par_semaine_counts)


