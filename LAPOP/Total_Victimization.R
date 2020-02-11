library(here)
library(ggplot2)
library(reshape2)

# Read in intent to migrate data
victim <- read.csv(here("LAPOP/Data/victimization_total.csv"), check.names = FALSE)

wola_blue <- '#00b5ef'
wola_grey <- '#d5d0ca'
wola_text <- "#404042"

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

plot <- ggplot(victim, aes(factor(wave), perc_mig, fill = pais)) + 
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = c(wola_blue, wola_purple, wola_green)) +
  facet_grid(cols = vars(pais)) + 
  transition_reveal(wave) +
  labs(title = "Respondents in all three countries reported rising\n levels of crime victimization",
       subtitle = "Self-reported victimization rates peaked in 2016",
       caption = "Source: Question vic1ext in LAPOP surveys, asking if survey respondent have been the victims of crime in the last 12 months") +
  xlab('year') + ylab('Percent reporting victimization of a crime') 

plot + wola_theme + theme(legend.position = 'none')
