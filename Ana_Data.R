# Ce fichier représente la partie 3 (Analyse) du projet Big Data

# Load data preparation script
source("Prepa_Data.R")

# =========================================================================== MC
# Tableaux croisées et des tests d’indépendance du chi2 sur les tableaux entre les différentes variables

# Displaying the cross-tabulation of descr_athmo based on descr_grav
table1 <- xtabs(~ descr_athmo + descr_grav, data)
print(table1)

# Displaying the cross-tabulation of descr_lum based on descr_etat_surf
table2 <- xtabs(~ descr_lum + descr_etat_surf, data)
print(table2)

# Displaying the cross-tabulation of descr_agglo based on descr_type_col
table3 <- xtabs(~ descr_agglo+ descr_type_col, data)
print(table3)

# Displaying chi-square independence tests
chisq.test(table1)
chisq.test(table2)
chisq.test(table3)

# =========================================================================== MC
# Représentation graphiqueme des tableaux (mosaicplot)

#Affichage des mosaicplot
mosaicplot(table1, main = "descr_atmo en fonction de la descr_grav")
mosaicplot(table2, main = "descr_lum en fonction de la descr_etat_surf")
mosaicplot(table3, main = "desc_agglo en fonction de la desc_type_col")


# =========================================================================== MB
# Régression linéaire du nombre d'accident par mois

# Number of accidents per month
accidentsPerMonth <- data %>%
  group_by(month = floor_date(date, 'month')) %>%
  summarise(accidentCount = n())


# Performing linear regression
regressionMonth <- lm(accidentCount ~ month, data = accidentsPerMonth)

# Displaying linear regression results
summary(regressionMonth)


# Plotting the linear regression graph for accidents per month
plot(accidentCount ~ month, data = accidentsPerMonth, 
     xlab = "Mois", ylab = "Nombre d'accidents",
     main = "Régression linéaire - Accidents par mois")

# Adding the regression line
abline(regressionMonth, col = "red")


# ============================================================================ MB
# Régression linéaire du nombre d'accident par semaine


# Number of accidents per week
accidentsPerWeek <- data %>%
  group_by(week = floor_date(date, 'week')) %>%
  summarise(accidentCount = n())


# Performing linear regression
regressionWeek <- lm(accidentCount ~ week, data = accidentsPerWeek)

# Displaying linear regression results
summary(regressionWeek)

# Tracer le graphique de la régression linéaire pour les accidents par semaine
plot(accidentCount ~ week, data = accidentsPerWeek, 
     xlab = "Semaine", ylab = "Nombre d'accidents",
     main = "Régression linéaire - Accidents par semaine")

# Adding the regression line
abline(regressionWeek, col = "red")

 