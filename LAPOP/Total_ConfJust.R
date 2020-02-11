library(here)
library(ggplot2)
library(reshape2)
library(gganimate)
library(dplyr)

# Read in intent to migrate data
institutions <- read.csv(here("LAPOP/Data/confjust_total.csv"), check.names = FALSE)

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
                    axis.title.y = element_text(color=wola_text, size=10, margin = margin(r=10), face = "italic"),
                    axis.ticks = element_blank(),
                    plot.caption = element_text(size = 8, margin = margin(t = 15), color = wola_text, face = 'italic'),
                    legend.title = element_text(face='bold', size=8),
                    legend.key.size = unit(0.75, 'cm'))

plot <- ggplot(institutions, aes(factor(wave), perc_mig, fill = pais)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = c(wola_blue, wola_purple, wola_green)) +
  facet_grid(cols = vars(pais)) + 
  labs(title = "Respondents in all three countries reported high levels of little to no confidence \n in the efficacy of justice institutions",
       subtitle = "Salvadorians lack more confidence in justice institutions than Guatemalans or Hondurans",
       caption = "Source: Question AOJ12 in LAPOP surveys, asking if survey respondent trusts the judicial system to punish someone who robbed or assaulted them") +
  xlab('year') + ylab('Percent repoting little to no confidence in justice') 

plot + wola_theme + theme(legend.position = 'none')
