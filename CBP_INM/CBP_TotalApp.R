library(here)
library(ggplot2)
library(gganimate)
library(dplyr)
library(magick)

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



# Read in data, fix NAs, keep total rows
cbp <- read.csv(here("CBP_INM/Data/Total_Apprehensions.csv"), check.names = FALSE)
cbp[is.na(cbp)] <- 0
cbp <- subset(cbp, `Fiscal Year` >= 1980)

# Select columns we need and rename
cbpfin <- cbp %>%
  select(`Fiscal Year`, `Southwest Border Total`) %>%
  rename(total = `Southwest Border Total`, year = `Fiscal Year`)

# Get rid of commas, which throw off the rank
cbpfin$total <- as.numeric(gsub(",","",cbpfin$total))

# Add 2019 value, which isn't included in this dataset: 851,508
cbpfinal <- rbind(cbpfin, c(2019, 851508))

# This is best told as a line graph
plot <- ggplot(cbpfinal, aes(x=year, y=total, group=1)) + 
  geom_line(color=wola_blue) +
  scale_x_continuous(breaks = seq(1980, 2019, 5)) + 
  geom_point(data = cbpfinal %>% filter(year == 2019)) +
  geom_text(data = cbpfinal %>% filter(year == 2019), aes(label = '851,508'), vjust = -1, nudge_y = 1, size = 3) +
  geom_point(data = cbpfinal %>% filter(year == 2011)) +
  geom_text(data = cbpfinal %>% filter(year == 2011), aes(label = '327,577'), vjust = 2, nudge_y = 0, size = 3) +
  geom_point(data = cbpfinal %>% filter(year == 2007)) +
  geom_text(data = cbpfinal %>% filter(year == 2007), aes(label = '858,638'), hjust = -.25, nudge_x = 0, size = 3) +
  geom_point(data = cbpfinal %>% filter(year == 2000)) +
  geom_text(data = cbpfinal %>% filter(year == 2000), aes(label = '1,643,679'), hjust = -.25, nudge_x = 0, size = 3) +
  geom_hline(yintercept = 1000000, linetype = 'dashed', color = wola_grey, show.legend = TRUE) +
  annotate('text', x = 2014, y = 1020000, label = 'italic("1 million apprehensions")', size = 4, color = wola_grey, parse=TRUE)

labels <- labs(title = 'Apprehensions at the Southwest border are at their highest \nsince 2007',
               subtitle = 'Prior to 2007, apprehensions were routinely much higher',
               caption = 'Source: CBP Apprehension Data (1980-2019)')

final <- plot + labels + wola_theme

ggsave("Graphs/Yearly_App.jpeg", width = 8, height = 6)
