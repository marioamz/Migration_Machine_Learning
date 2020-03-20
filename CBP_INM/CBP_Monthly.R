library(here)
library(ggplot2)
library(gganimate)
library(dplyr)
library(reshape2)
library(tidyr)
library(viridis)

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
                    axis.line.y = element_line(colour = wola_text),
                    axis.text.x = element_text(size = 8, color = wola_text),
                    axis.text.y = element_text(size = 8, color = wola_text),
                    axis.title.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    axis.ticks.x = element_blank(),
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

# Get rid of commas, which throw off the values, and add rest of 2019 data
fin$total <- as.numeric(gsub(",","",fin$total))

fin <- rbind(fin, `241` = c('October', 2019, 35406))
fin <- rbind(fin, `242` = c('November', 2019, 33517))
fin <- rbind(fin, `243` = c('December', 2019, 32854))

fin$total <- as.numeric(fin$total)
fin$month <- factor(fin$month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
fin <- subset(fin, year!= 1999)

# I want to show trends over months in the same year 
touse <- fin %>%
  group_by(year) %>%
  summarise(max = max(total)) %>%
  left_join(fin, by = c('year')) %>%
  mutate(diff = max - total) %>%
  select(month, year, diff, total)
  
touse <- transform(touse, sqrt = sqrt(diff))
sqfin <- transform(fin, sqrt = sqrt(total))

# Now we can finally plot the heatmap

plot <- ggplot(touse, aes(month, reorder(year, desc(year)), fill=total)) +
  geom_tile(aes(fill = total)) + 
  scale_fill_distiller(palette = "RdYlBu") + 
  scale_x_discrete(position = "top") +
  labs(title = "Monthly southwest border apprehensions data suggest \nmigration tends to increase in warmer weather",
       subtitle = "Historically, March, April, and May have been peak migration months", 
       caption = "Source: CBP Apprehensions Data",
       fill = "Monthly \nApprehensions") 

save <- plot + wola_theme
save

ggsave("Graphs/Monthly_Heatmap.jpeg", width = 8, height = 6)
