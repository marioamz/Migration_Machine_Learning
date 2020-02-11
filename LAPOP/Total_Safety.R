library(here)
library(ggplot2)
library(reshape2)
library(waffle)
library(gganimate)
library(tidyverse)

# Read in intent to migrate data
safety <- read.csv(here("LAPOP/Data/percsafety_total.csv"), check.names = FALSE)

safety <- safety %>%
  select(wave, pais, perc_mig) %>%
  mutate(no_perc = 100 - perc_mig) %>%
  select(wave, pais, perc_mig, no_perc)

safety <- melt(safety, id.vars = c('wave', 'pais'))

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
                    plot.caption = element_text(size = 8, margin = margin(t = 15), color = wola_text, face = 'italic', hjust = 0),
                    legend.title = element_text(face='bold', size=8),
                    legend.key.size = unit(0.75, 'cm'))

honduras <- safety %>%
  filter(pais == 'Honduras')

guate <- safety %>%
  filter(pais == 'Guatemala')

els <- safety %>% 
  filter(pais == 'El Salvador')

plothond <- ggplot(honduras, aes(fill=variable, values=value)) +
  geom_waffle(color = "white", size=1.125, n_rows = 5) +
  facet_wrap(~wave, ncol=1) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(
    title = "Perception of safety in respondent's neighborhood \npeaked in 2016 in Honduras",
    subtitle = "Data from 2018 suggests a decrease",
    caption = "Source: Question aoj11 in LAPOP surveys, asking if survey respondent feels safe in their neighborhood for robberies or assaults") + 
  theme_enhance_waffle() +
  scale_fill_manual(name = 'Perception of Safety in Neighborhood', labels = c("Feel unsafe (a little to very)", "Feel safe (a little to very)"), values = c(wola_green, wola_grey))


plotguate <- ggplot(guate, aes(fill=variable, values=value)) +
  geom_waffle(color = "white", size=1.125, n_rows = 5) +
  facet_wrap(~wave, ncol=1) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(
    title = "Perception of safety in respondent's neighborhood has \nrisen notably over time in Guatemala",
    subtitle = "More than 50% of respondents feel unsafe in their neighborhood",
    caption = "Source: Question aoj11 in LAPOP surveys, asking if survey respondent feels safe in their neighborhood for robberies or assaults") + 
  theme_enhance_waffle() +
  scale_fill_manual(name = 'Perception of Safety in Neighborhood', labels = c("Feel unsafe (a little to very)", "Feel safe (a little to very)"), values = c(wola_purple, wola_grey))


plotels <- ggplot(els, aes(fill=variable, values=value)) +
  geom_waffle(color = "white", size=1.125, n_rows = 5) +
  facet_wrap(~wave, ncol=1) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  coord_equal() +
  labs(
    title = "Perception of safety in respondent's neighborhood \nhas stayed relatively constant over time in El Salvador",
    subtitle = "Roughly 40% of Salvadorians consistently report feeling unsafe \nin their neighborhoods",
    caption = "Source: Question aoj11 in LAPOP surveys, asking if survey respondent feels safe from robberies and assaults in their neighborhood") + 
  theme_enhance_waffle() +
  scale_fill_manual(name = 'Perception of Safety in Neighborhood', labels = c("Feel unsafe (a little to very)", "Feel safe (a little to very)"), values = c(wola_blue, wola_grey))

finalels <- plotels + wola_theme
ggsave("Graphs/Total_ELS_PercSafe.jpeg", width = 8, height = 6)

finalguate <- plotguate + wola_theme
ggsave("Graphs/Total_GT_PercSafe.jpeg", width = 8, height = 6)

finalhond <- plothond + wola_theme
ggsave("Graphs/Total_HN_PercSafe.jpeg", width = 8, height = 6)

