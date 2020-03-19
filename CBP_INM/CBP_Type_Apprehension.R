library(here)
library(ggplot2)
library(gganimate)
library(dplyr)
library(reshape2)
library(tidyr)
library(zoo)
library(ggrepel)

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

# Adding the rest of the months
fin$total <- as.numeric(gsub(",","",fin$total))

fin <- rbind(fin, `241` = c('October', 2019, 35406))
fin <- rbind(fin, `242` = c('November', 2019, 33517))
fin <- rbind(fin, `243` = c('December', 2019, 32854))
fin <- rbind(fin, `244` = c('January', 2020, 29206))
fin <- rbind(fin, `245` = c('February', 2020, 30068))

fin$total <- as.numeric(fin$total)
fin$month <- factor(fin$month, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
fin <- subset(fin, year >= 2013)

# Getting Family and UAC data repeating same steps as above for each
uac <- read.csv(here("CBP_INM/Data/CBP_Monthly_UAC.csv"), check.names = FALSE)
fam <- read.csv(here("CBP_INM/Data/CBP_Monthy_Families.csv"), check.names = FALSE)

colnames(uac) <- c('filter', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13')
colnames(fam) <- c('filter', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13')


uacfin <- uac %>%
  filter(filter %in% c("Southwest Border", "SECTOR"))

famfin <- fam %>%
  filter(filter %in% c("Southwest Border", "SECTOR"))

uacsector <- uacfin %>%
  filter(filter == 'SECTOR')
uacy <- melt(uacsector, id.vars = "filter")

totaluac <- uacfin %>%
  filter(filter == 'Southwest Border')
uacx <- melt(totaluac, id.vars = 'filter')

finaluac <- cbind(uacy, uacx)
colnames(finaluac) <- c('filter', 'variable', 'month', 'border', 'value', 'UAC')


famsector <- famfin %>%
  filter(filter == 'SECTOR')
famy <- melt(famsector, id.vars = "filter")

famtotal <- famfin %>%
  filter(filter == 'Southwest Border')
famx <- melt(famtotal, id.vars = 'filter')

finalfam <- cbind(famy, famx)
colnames(finalfam) <- c('filter', 'variable', 'month', 'border', 'value', 'Family')

# Finally
finaluac <- finaluac %>%
  select(month, UAC) %>%
  filter(month != "Yearly")

finuac <- finaluac %>% 
  separate(month, c('month', 'year'))

finuac <- finuac[complete.cases(finuac),]

finuac <- finuac[0:84,]

# Finally
finalfam <- finalfam %>%
  select(month, Family) %>%
  filter(month != "Yearly")

finfam <- finalfam %>% 
  separate(month, c('month', 'year'))

finfam <- finfam[complete.cases(finfam),]

finfam <- finfam[0:84,]

# Removing 2012 observations and then binding
finfam <- finfam %>%
  filter(year != 2012)

finuac <- finuac %>%
  filter(year != 2012)

finfam <- rbind(finfam, `82` = c('October', 2019, 9723))
finfam <- rbind(finfam, `83` = c('November', 2019, 8999))
finfam <- rbind(finfam, `84` = c('December', 2019, 8595))
finfam <- rbind(finfam, `85` = c('January', 2020, 5163))
finfam <- rbind(finfam, `86` = c('February', 2020, 4610))

finuac <- rbind(finuac, `82` = c('October', 2019, 2841))
finuac <- rbind(finuac, `83` = c('November', 2019, 3309))
finuac <- rbind(finuac, `84` = c('December', 2019, 3229))
finuac <- rbind(finuac, `85` = c('January', 2020, 2682))
finuac <- rbind(finuac, `86` = c('February', 2020, 3076))

# join 
inner <- merge(finfam, finuac, by = c('month', 'year'))
touse <- merge(inner, fin, by = c('month', 'year'))
touse$UAC <- as.numeric(gsub(",","",touse$UAC))
touse$Family <- as.numeric(gsub(",","",touse$Family))

# Now to get the number of single adults from the difference between the two
touse <- touse %>%
  mutate(Adults = total - (Family + UAC))

melted <- melt(touse)
melted$date <- as.yearmon(paste(melted$year, melted$month), "%Y %b")
melted <- melted %>%
  filter(variable != 'total')
melted <- transform(melted, date = as.Date(date, frac = 1))

melted <- melted %>%
  mutate(rank = rank(date)) %>%
  arrange(date)


# FINALLY A PLOT

plot <- ggplot(melted, aes(date, value, group=variable)) +
  geom_line(aes(color = variable), size = 1) + 
  geom_path() +
  geom_segment(aes(xend = as.Date("2020-03-15", "%Y-%m-%d"), yend = value), linetype = 2, colour = wola_grey) +
  scale_color_manual(values = c(wola_blue, wola_purple, wola_green)) + 
  geom_point(size = 2) + 
  geom_text_repel(aes(x = as.Date("2020-03-15", "%Y-%m-%d"), label = variable), hjust = -2) + 
  transition_reveal(date) + 
  view_follow() +
  coord_cartesian(clip = 'off') + 
  labs(title = 'Between 2018 and 2019, CBP apprehended more family units than ever before',
       subtitle = 'Historically, single adults have been the majority of apprehensions',
       caption = "Source: CBP Southwest Border Apprehension Data") 

animation <- plot + wola_theme + theme(legend.position = 'none')

final_animation<-animate(animation,50,fps = 10,duration = 10, width = 800, height = 600, renderer = gifski_renderer())

final_animation 

anim_save('Graphs/Type_Apprehension.gif', final_animation)
