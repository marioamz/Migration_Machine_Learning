library(here)
library(ggplot2)
library(reshape2)
library(gganimate)

# Read in intent to migrate data
income <- read.csv(here("LAPOP/Data/income_total.csv"), check.names = FALSE)

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
                    axis.title.x = element_text(color=wola_text, size=10, margin = margin(t=20), face = "italic"),
                    axis.text.x = element_text(angle = 0, hjust=1),
                    axis.title.y = element_blank(), 
                    axis.ticks = element_blank(),
                    plot.caption = element_text(size = 8, margin = margin(t = 15), color = wola_text, face = 'italic'),
                    legend.title = element_text(face='bold', size=8),
                    legend.key.size = unit(0.75, 'cm'))

plot <- ggplot(income, aes(pais, perc_mig, fill = pais)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = c(wola_blue, wola_purple, wola_green)) +
  transition_time(wave) + 
  coord_flip() + 
  labs(title = "Rising numbers of Hondurans, Guatemalans \n are reporting their income is not enough",
       subtitle = "Year: {frame_time}",
       caption = "Source: Question Q10D in LAPOP surveys, asking if the total salary and income received in respondents' household is enough") +
  xlab('year') + ylab('percent reporting that income is not enough') 

plot + wola_theme + theme(legend.position = 'none')
