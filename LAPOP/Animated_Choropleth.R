library(here)
library(ggplot2)
library(sf)
library(tigris)
library(gganimate)

# Read in data
elsdf <- read.csv('Data/El Salvador_intent.csv', check.names = FALSE)
guatedf <- read.csv('Data/Guatemala_intent.csv', check.names = FALSE)
honddf <- read.csv('Data/Honduras_intent.csv', check.names = FALSE)


# Read in shape files
honduras_shape <- st_read('Data/HND_adm/HND_adm1.shp')
guate_shape <- 
els_shape <- 
  
# Merge
final_hond <- merge(honddf, honduras_shape, by.x = "state", by.y = "NAME_1")

# Map
final <- ggplot() + 
  geom_sf(data = final_hond) + 
  geom_sf(data = final_hond, aes(fill = perc_mig)) +
  transition_time(year) 

animate(final)
  
