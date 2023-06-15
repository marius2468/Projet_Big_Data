
source("Prepa_Data.R")



# Affichage du tableau croisé de la descr_atmo en fonction de la descr_grav
tableau <- table(data$descr_athmo, data$descr_grav)
print(tableau)

#Affichage du tableau croisé de la descr_cat_veh en fonction de la descr_etat_surf
tableau_2 <- table(data$descr_cat_veh, data$descr_etat_surf)
print(tableau_2)

#Affichage du tableau croisé de la desc_agglo en fonction de la desc_type_col
tableau_3 <-table(data$descr_agglo, data$descr_type_col)
print(tableau_3)

#Affichage des tests d'indépendances du chi2
chisq.test(tableau)
chisq.test(tableau_2)
chisq.test(tableau_3)

#Affichage des mosaicplot
mosaicplot(tableau, color = "blue", main = "descr_cat_veh en fonction de la descr_etat_surf")
mosaicplot(tableau_2, color ="green", main = "descr_cat_veh en fonction de la descr_etat_surf")
mosaicplot(tableau_3, color ="purple", main = "desc_agglo en fonction de la desc_type_col")

# ==============================================================================
# Régression linéaire du nombre d'accident par mois

# Number of accidents per month
accidentsPerMonth <- data %>%
  group_by(month = floor_date(date, 'month')) %>%
  summarise(accidentCount = n())


# Réalisation de la regression linéaire
regressionMonth <- lm(accidentCount ~ month, data = accidentsPerMonth)

# Affichage des résultats de la régression linéaire
summary(regressionMonth)


# Tracer le graphique de la régression linéaire pour les accidents par mois
plot(accidentCount ~ month, data = accidentsPerMonth, 
     xlab = "Mois", ylab = "Nombre d'accidents",
     main = "Régression linéaire - Accidents par mois")

# Ajouter la ligne de régression
abline(regressionMonth, col = "red")


# ==============================================================================
# Régression linéaire du nombre d'accident par semaine


# Number of accidents per week
accidentsPerWeek <- data %>%
  group_by(week = floor_date(date, 'week')) %>%
  summarise(accidentCount = n())


# Réalisation de la régression linéaire
regressionWeek <- lm(accidentCount ~ week, data = accidentsPerWeek)

# Affichage des résultats de la régression linéaire
summary(regressionWeek)

# Tracer le graphique de la régression linéaire pour les accidents par semaine
plot(accidentCount ~ week, data = accidentsPerWeek, 
     xlab = "Semaine", ylab = "Nombre d'accidents",
     main = "Régression linéaire - Accidents par semaine")

# Ajouter la ligne de régression
abline(regressionWeek, col = "red")

 