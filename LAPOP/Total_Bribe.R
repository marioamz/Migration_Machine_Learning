library(here)
library(ggplot2)
library(reshape2)
library(gganimate)

# Read in intent to migrate data
bribe <- read.csv(here("LAPOP/Data/bribe_total.csv"), check.names = FALSE)

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
                    axis.title.x = element_text(color=wola_text, size=10, margin = margin(t=20)),
                    axis.text.x = element_text(angle = 0, hjust=1),
                    axis.title.y = element_text(color=wola_text, size=10, margin = margin(r=10)),
                    axis.ticks = element_blank(),
                    plot.caption = element_text(size = 8, margin = margin(t = 15), color = wola_text, face = 'italic'),
                    legend.title = element_text(face='bold', size=8),
                    legend.key.size = unit(0.75, 'cm'))

plot <- ggplot(bribe, aes(x=wave, y=perc_mig, group = pais, label = pais)) + 
  geom_line(aes(color = pais), size = 1) + scale_color_manual(values = c(wola_blue, wola_purple, wola_green)) + 
  geom_point() + geom_text() + transition_reveal(wave) + 
  labs(title = 'Region-wide, people self-report paying bribes \n to police or public officials less frequently',
       subtitle = 'Decreases are evident in Honduras and Guatemala, \n reporting in El Salvador has remained constant',
       caption = "Source: EXC2 and EXC6 in LAPOP surveys, \n asking if police or public officials have asked the survey taker for a bribe in the last 12 months") + 
       xlab('Year') + ylab('Percent responding "Yes"') 

plot + wola_theme + theme(legend.position = "none")
