library(clairesfirstpackage)
library(dplyr)

data <- df

latitudes <- 50.88
longitudes <- -2.42

df$lat <- latitudes
df$lon <- longitudes



carbon <- total_tree_carbon(data = df,
                            dbh_col = 'dbh',
                            genus_col = 'genus',
                            species_col = 'species',
                            lat_col = 'lat',
                            lon_col = 'lon',
                            carbon = 0.5)

data$biomass <- allodb::get_biomass(dbh = data[["dbh"]],
                       genus = data[['genus']],
                       species = data[['species']],
                       coords = data[,c("lon", "lat")])

data <- data %>%
  mutate(carbon = biomass*0.5)

total_carbon <- data %>%
  summarise(total_carbon = sum(carbon))
