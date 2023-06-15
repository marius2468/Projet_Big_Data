library("foreign")
library("tidyverse")


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
mosaicplot(tableau)
mosaicplot(tableau_2)
mosaicplot(tableau_3)

# Construit le tableau des mois en fonction du nombre d'accidents 
accidents_par_mois <- as.data.frame(accidents_par_mois_counts)
print(accidents_par_mois)
# Désignation des colonnes par leur signification
colnames(accidents_par_mois) <- c("Mois", "Nombre_accidents")

# Création de la variable mois
accidents_par_mois$Mois <- as.yearmon(accidents_par_mois$Mois, "%Y-%m")
print(accidents_par_mois$Mois)

# Réalisation de la regression linéaire
regression_mois <- lm(Nombre_accidents ~ Mois, data = accidents_par_mois)


# Construit le tableau des semaines en fonction du nombre d'accidents 
accidents_par_semaine <- as.data.frame(accidents_par_semaine_counts)
print(accidents_par_semaine)
# Désignation des colonnes par leur signification
colnames(accidents_par_semaine) <- c("Semaine", "Nombre_accidents")

# Réalisation de la régression linéaire
regression_semaine <- lm(Nombre_accidents ~ Semaine, data = accidents_par_semaine)

# Affichage des résultats de la régression linéaire
summary(regression_mois)

# Affichage des résultats de la régression linéaire
summary(regression_semaine)


 