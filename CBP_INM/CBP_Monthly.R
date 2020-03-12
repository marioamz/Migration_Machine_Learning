library(here)
library(ggplot2)
library(gganimate)
library(dplyr)
library(magick)
library(reshape2)
library(tidyr)

# WOLA theme and colors
wola_blue <- '#00b5ef'
wola_grey <- '#d5d0ca'
wola_text <- "#404042"
wola_purple <- '#603F99'
wola_green <- '#65BD60'

wola_theme <- theme(text = element_text(family='Arial'),
                    plot.background = element_blank(),
                    panel.background = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_blank(),
                    plot.title = element_text(size = 18, color = wola_text, face = 'bold', margin = margin(b = 10)),
                    plot.subtitle = element_text(size = 16, color = wola_text, face = 'italic', margin = margin(b = 25)), 
                    axis.title.x = element_blank(),
                    axis.line.x = element_line(colour = wola_text),
                    axis.text.x = element_text(size = 8, color = wola_text),
                    axis.text.y = element_blank(),
                    axis.title.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    plot.caption = element_text(size = 8, margin = margin(t = 15), color = wola_text, face = 'italic'),
                    legend.title = element_text(face='bold', size=8),
                    legend.key.size = unit(0.75, 'cm'))


# Read in data, fix NAs, rename columns
cbp <- read.csv(here("CBP_INM/Data/CBP_Monthly.csv"), check.names = FALSE)
cbp[is.na(cbp)] <- 0
colnames(cbp) <- c('filter', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13')

# Get the rows I need
cbpfin <- cbp %>%
  filter(filter %in% c("Southwest Border", "SECTOR"))

# Pull out the sectors I need into one dataframe and melt
sector <- cbpfin %>%
  filter(filter == 'SECTOR')

y <- melt(sector, id.vars = "filter")

# Pull out the data I need into one dataframe and melt
total <- cbpfin %>%
  filter(filter == 'Southwest Border')

x <- melt(total, id.vars = 'filter')

# They preserved order, therefore the column bind
final <- cbind(y, x)
colnames(final) <- c('filter', 'variable', 'month', 'border', 'value', 'total')

# Finally
final <- final %>%
  select(month, total) %>%
  filter(month != "Yearly")

fin <- final %>% 
  separate(month, c('month', 'year'))

fin <- fin[0:240,]

# Get rid of commas, which throw off the values
fin$total <- as.numeric(gsub(",","",fin$total))

# Now we can finally plot
