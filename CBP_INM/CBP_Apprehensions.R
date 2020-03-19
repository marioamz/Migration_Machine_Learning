library(here)
library(ggplot2)
library(gganimate)
library(dplyr)
library(magick)

# Read in data, fix NAs, get rid of total rows
cbp <- read.csv(here("CBP_INM/Data/CBP_data.csv"), check.names = FALSE)
cbp[is.na(cbp)] <- 0
cbp <- subset(cbp, Country != 'TOTAL')

# Select columns we need and rename
cbpfin <- cbp %>%
  select(`Country`, `Year`, `SBO Total`) %>%
  rename(total = `SBO Total`)

# Get rid of commas, which throw off the rank
cbpfin$total <- as.numeric(gsub(",","",cbpfin$total))

# Rank the top 10 per year
cbpfinal <- cbpfin %>%
  group_by(Year) %>%  
  arrange(total, Year, Country) %>%
  mutate(rank = rank(-total), 
         Value_rel = total/total[rank==1],
         Value_lbl = paste0(" ",round(total/1e9))) %>%
  group_by(Country) %>% 
  filter(rank <=10)

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
                    axis.text.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.ticks = element_blank(),
                    plot.caption = element_text(size = 8, margin = margin(t = 15), color = wola_text, face = 'italic'),
                    legend.title = element_text(face='bold', size=8),
                    legend.key.size = unit(0.75, 'cm'))

# Plot all the graphs
plot = ggplot(cbpfinal, aes(rank, group = Country, 
                                  fill = as.factor(Country), color = as.factor(Country))) +
  geom_tile(aes(y = total/2,
                height = total,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Country, " ")), vjust = 0.2, hjust = 1) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) + wola_theme + 
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

# Animate by year
anim = plot + transition_states(Year) +
  labs(title = 'Central American migration has increased exponentially in the last decade, \nwhile Mexican migration has decreasd',
       subtitle  =  "Top 10 countries of origin for CBP apprehensions: {closest_state}",
       caption  = "Source: Customs and Border Protection Data (2007-2019)")

final_animation<-animate(anim,50,fps = 15,duration = 15, width = 800, height = 600, renderer = gifski_renderer())

final_animation

# Save
anim_save('Graphs/CBP_Apps.gif', final_animation)

