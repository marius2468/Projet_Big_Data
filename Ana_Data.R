source("Prepa_Data.R")

# Affichage du tableau croisé de la descr_athmo en fonction de la descr_grav
tableau <- xtabs(~ descr_athmo + descr_grav, data)
print(tableau)

#Affichage du tableau croisé de la descr_lum en fonction de la descr_etat_surf
tableau_2 <- xtabs(~ descr_lum + descr_etat_surf, data)
print(tableau_2)

#Affichage du tableau croisé de la descr_agglo en fonction de la descr_type_col
tableau_3 <- xtabs(~ descr_agglo+ descr_type_col, data)
print(tableau_3)

#Affichage des tests d'indépendances du chi2
chisq.test(tableau)
chisq.test(tableau_2)
chisq.test(tableau_3)

#Affichage des mosaicplot
mosaicplot(tableau, main = "descr_atmo en fonction de la descr_grav")
mosaicplot(tableau_2, main = "descr_lum en fonction de la descr_etat_surf")
mosaicplot(tableau_3, main = "desc_agglo en fonction de la desc_type_col")


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

 