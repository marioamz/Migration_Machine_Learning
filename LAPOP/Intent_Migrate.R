library(here)
library(ggplot2)
library(reshape2)

# Read in intent to migrate data
df <- read.csv(here("LAPOP/Data/intent_migrate.csv"), check.names = FALSE)
final <- melt(df)

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

plot <- ggplot(final, aes(variable, country, fill=value)) +
  geom_tile(aes(fill = value)) + 
  geom_text(aes(label=paste(round(value,1),"%"))) +
  scale_fill_gradient(name = "% intending to live or work \n in another country in 3 years",
                       low = "#e3f2fd",
                       high = wola_blue) +
labs(title = "Central Americans are increasingly likely to indicate they'll \n live or work in a foreign country in the next three years",
     subtitle = "Hondurans' intentions to work or live elsewhere surpass other \n Northern Triangle countries", 
     caption = "Source: Q14 in LAPOP surveys, asking if survey taker intends to live or work in another country in the next three years") +
  xlab('Year')

save <- plot + wola_theme

ggsave("Graphs/Intent_Heatmap.jpeg", width = 8, height = 6)
