# Load data preparation script
source("Prepa_Data.R")

# ==============================================================================
# Représentation graphique du nombre d’accidents en fonction des conditions atmosphériques


# Create histogram of accidents by weather conditions
weatherHistogram <- data %>%
  group_by(descr_athmo) %>%
  summarise(accidentCount = n())

# Rename weather condition categories
weatherHistogram <- weatherHistogram %>%
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

# Plot the histogram
barplot(weatherHistogram$accidentCount,
        names.arg = weatherHistogram$descr_athmo,
        main = "Number of accidents by weather conditions",
        col = "lightblue",
        border = "black",
        las = 2)


# ==============================================================================
# Représentation graphique du nombre d’accidents enfonction de la description de la surface


# Create histogram of accidents by road surface conditions
surfaceHistogram <- data %>%
  group_by(descr_etat_surf) %>%
  summarise(accidentCount = n())

# Rename road surface condition categories
surfaceHistogram <- surfaceHistogram %>%
  mutate(descr_etat_surf = case_when(
    descr_etat_surf == -1 ~ "Non renseigné",
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

# Plot the histogram
barplot(surfaceHistogram$accidentCount,
        names.arg = surfaceHistogram$descr_etat_surf,
        main = "Number of accidents by road surface conditions",
        col = "lightblue",
        border = "black",
        las = 2)


# ==============================================================================
# Représentation graphique du nombre d’accidents selon la gravité


# Create histogram of accidents by severity
severityHistogram <- data %>%
  group_by(descr_grav) %>%
  summarise(accidentCount = n())

# Rename severity categories
severityHistogram <- severityHistogram %>% 
  mutate(descr_grav = case_when(
    descr_grav == 1 ~ "Indemne",
    descr_grav == 2 ~ "Blessé léger",
    descr_grav == 3 ~ "Blessé hospitalisé",
    descr_grav == 4 ~ "Tué"
  ))

# Plot the histogram
barplot(severityHistogram$accidentCount,
        names.arg = severityHistogram$descr_grav,
        main = "Number of accidents by severity",
        col = "lightblue",
        border = "black",
        las = 0.5)


# ==============================================================================
# Représentation graphique du nombre d’accidents par tranches d’heure


# Create a table of accidents by hour
accidentsByHour <- table(format(data$date, "%H"))

# Convert the table to a dataframe
accidentsByHour <- as.data.frame(accidentsByHour)
names(accidentsByHour) <- c("Hour", "accidentCount")

# Plot the line graph of accidents by hour
plot(accidentsByHour$accidentCount, type = "l",
     xlab = "Hour",
     ylab = "Number of accidents",
     main = "Number of accidents by hour",
     xlim = c(0, 23),
     col = "blue", xaxt = "n")

# Add labels to the x-axis
axis(1, at = 0:23, labels = paste0(0:23, "h"))


# ==============================================================================
# Représentation graphique du nombre d’accidents par ville


# Calculate the number of accidents by city
accidentsByCity <- table(data$ville)

# Sort cities by the number of accidents in descending order
sortedCities <- sort(accidentsByCity, decreasing = TRUE)

# Select the top 20 cities with the highest number of accidents
top20Cities <- sortedCities[1:20]

# Plot the histogram of accidents for the top 20 cities
barplot(top20Cities, horiz = FALSE,
        ylab = "Number of accidents",
        main = "Number of accidents for the top 20 most affected cities",
        col = "lightblue",
        border = "black",
        las = 2)


# ==============================================================================
# Représentation graphique de la quantité d’accidents en fonction des tranches d’âges


# Create histogram of accidents by age group
ageHistogram <- data %>% mutate(ageGroup = case_when(
  age < 18 ~ "Moins de 18 ans",
  age >= 18 & age <= 24 ~ "18-24 ans",
  age >= 25 & age <= 34 ~ "25-34 ans",
  age >= 35 & age <= 44 ~ "35-44 ans",
  age >= 45 & age <= 54 ~ "45-54 ans",
  age >= 55 & age <= 64 ~ "55-64 ans",
  age >= 65 ~ "65 ans et plus"
))

ageHistogram <- ageHistogram %>%
  group_by(ageGroup) %>%
  summarise(accidentCount = n())

# Plot the histogram
barplot(ageHistogram$accidentCount,
        names.arg = ageHistogram$ageGroup,
        main = "Number of accidents by age group",
        col = "lightblue",
        border = "black",
        las = 2)

# ==============================================================================
# Représentation graphique de la moyenne mensuelle des accidents


# Create a new column 'month' to extract the month from the 'date' column
data$month <- format(data$date, "%Y-%m")

# Calculate the monthly average of accidents
monthlyAverage <- data %>%
  group_by(month) %>%
  summarise(averageAccidents = mean(n()))

# Plot the bar graph of monthly average accidents
barplot(monthlyAverage$averageAccidents,
        names.arg = monthlyAverage$month,
        main = "Monthly average accidents",
        col = "lightblue",
        border = "black",
        las = 2)


# ==============================================================================
# Représentation sous formes de carte de la quantité d’accidents enregistrés par région


# Load geographical data for France (regions)
regions <- st_read("regions.geojson")

regionInfo <- read.csv("anciennes-nouvelles-regions.csv", sep = ";", stringsAsFactors = FALSE)

# Rename columns for clarity
colnames(regionInfo) <- c("newCode", "newName", "oldCode", "oldName")

# Merge the 'data' dataframe with 'regionInfo' using the "REG" column in 'data'
# and the "oldCode" column in 'regionInfo'
data <- merge(data, regionInfo, by.x = "REG", by.y = "oldCode")

# Calculate the number of accidents by region
accidentsByRegion <- data %>%
  group_by(newCode) %>%
  summarise(accidentCount = n())

# Convert region codes to character type
accidentsByRegion$newCode <- as.character(accidentsByRegion$newCode)

# Join the regions data with the accident counts
regionsJoined <- inner_join(regions, accidentsByRegion, by = c("code" = "newCode"))

# Display the map
mapview(regionsJoined)


# ==============================================================================
# Représentation sous formes de carte de la quantité d’accidents enregistrés par départements



# Load geographical data for France (departments)
departments <- st_read("departements.geojson")

data$dpmt <- ifelse(nchar(data$id_code_insee) == 5, 
                    substr(data$id_code_insee, 1, 2), 
                    paste0("0", substr(data$id_code_insee, 1, 1)))

# Calculate the number of accidents by department
accidentsByDepartment <- data %>%
  group_by(dpmt) %>%
  summarise(accidentCount = n())

# Convert department codes to character type
accidentsByDepartment$dpmt <- as.character(accidentsByDepartment$dpmt)

# Join the departments data with the accident counts
departmentsJoined <- inner_join(departments, accidentsByDepartment, by = c("code" = "dpmt"))

# Display the map
mapview(departmentsJoined)



# ==============================================================================
# Représentation sous formes de carte du taux d’accidents graves enregistrés par région


severeAccidentsByRegion <- data %>%
  filter(descr_grav %in% c(3, 4)) %>%
  group_by(newCode) %>%
  summarise(severeAccidentCount = n())

rateAccidentsByRegion <- data %>%
  group_by(newCode) %>%
  summarise(accidentCount = n())

# Join severe accidents and total accidents by region
severeAccidentsByRegion <- left_join(severeAccidentsByRegion, rateAccidentsByRegion, by = "newCode")

# Calculate the percentage of severe accidents by region
severeAccidentsByRegion <- severeAccidentsByRegion %>%
  mutate(severeAccidentRate = (severeAccidentCount / accidentCount) * 100)

severeAccidentsByRegion$newCode <- as.character(severeAccidentsByRegion$newCode)

severeAccidentsByRegion <- severeAccidentsByRegion %>%
  mutate(severeAccidentRate = paste0(round(severeAccidentRate, 2), "%"))

regionsJoined <- inner_join(regions, severeAccidentsByRegion, by = c("code" = "newCode"))

# Display the map
mapview(regionsJoined)



# ==============================================================================
# Représentation sous formes de carte du taux d’accidents graves enregistrés par départements


severeAccidentsByDepartment <- data %>%
  filter(descr_grav %in% c(3, 4)) %>%
  group_by(dpmt) %>%
  summarise(severeAccidentCount = n())

# Join the departments data with the severe accident counts
departmentsJoined <- left_join(accidentsByDepartment, severeAccidentsByDepartment, by = "dpmt")

# Calculate the percentage of severe accidents by department
severeAccidentsByDepartment <- departmentsJoined %>%
  mutate(severeAccidentRate = (severeAccidentCount / accidentCount) * 100)

severeAccidentsByDepartment <- severeAccidentsByDepartment %>%
  mutate(severeAccidentRate = paste0(round(severeAccidentRate, 2), "%"))

print(severeAccidentsByDepartment)

departmentsJoined <- inner_join(departments, severeAccidentsByDepartment, by = c("code" = "dpmt"))

# Display the map
mapview(departmentsJoined)
