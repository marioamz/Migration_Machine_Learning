library(here)
library(ggplot2)
library(reshape2)
library(gghighlight)
library(gganimate)

# Read in intent to migrate data
services <- read.csv(here("LAPOP/Data/services_total.csv"), check.names = FALSE)

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
                    axis.title.y = element_text(color=wola_text, size=10, margin = margin(r=20), face = "italic"),
                    axis.ticks = element_blank(),
                    plot.caption = element_text(size = 8, margin = margin(t = 15), color = wola_text, face = 'italic'),
                    legend.title = element_text(face='bold', size=8),
                    legend.key.size = unit(0.75, 'cm'))

plot <- ggplot(services, aes(x=wave, y=perc_mig, group = pais, label = pais)) + 
  geom_line(aes(color = pais), size = 1) + scale_color_manual(values = c(wola_grey, wola_purple, wola_grey)) + 
  geom_text() + transition_reveal(wave) +
  labs(title = 'Guatemalans increasingly think local \n government services are poor',
       subtitle = 'Hondurans and Salvadorians are similarly frustrated \n with local services, but not as drastically',
       caption = "Source: SGL1 in LAPOP surveys, \n asking people's assessment of municipal services") + 
  xlab('Year') + ylab('Percent responding "Bad" or "Very Bad"') 

animation <- plot + wola_theme + theme(legend.position = "none")

anim <- animate(animation)

anim_save('Graphs/Total_Services.gif', anim)
