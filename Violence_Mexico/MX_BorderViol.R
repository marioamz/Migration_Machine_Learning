library(here)
library(ggplot2)
library(gghighlight)
library(dplyr)
library(tidyr)
library(reshape2)

# Load in theme and colors
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

# Read in data
pop <- read.csv(here("Violence_Mexico/Data/muni_pob.csv"), check.names = FALSE, skip = 4)
viol <- read.csv(here("Violence_Mexico/Data/Violence_data.csv"), check.names = FALSE)

# Set total number of Mexicans in 2015 and total number of women in 2015
total_pop <- sum(pop$total, na.rm = TRUE)
total_muj <- sum(pop$mujeres, na.rm = TRUE)

# Subset only for certain types of violence we're checking for and sum across rows
viol <- subset(viol, `Subtipo de delito` == 'Homicidio doloso' | 
                 `Subtipo de delito` == 'Feminicidio' |
                 `Subtipo de delito` == 'Secuestro' |
                 `Subtipo de delito` == 'Abuso sexual' |
                 `Subtipo de delito` == 'Acoso sexual' |
                 `Subtipo de delito` == 'Hostigamiento sexual' |
                 `Subtipo de delito` == 'Violación simple' |
                 `Subtipo de delito` == 'Violación equiparada')

viol$sums <- rowSums(viol[,10:21])

# Sum all homicide, femicide, and kidnapping rows; and join
final <- viol %>%
  group_by(Entidad, Municipio,`Bien jurídico afectado`, `Cve. Municipio`, `Subtipo de delito`) %>%
  summarise(total_sum = sum(sums), 
            total_sex = sum(sums[`Bien jurídico afectado`=='La libertad y la seguridad sexual'])) %>%
  left_join(pop, by = c(`Cve. Municipio` = 'cve_inegi')) %>%
  select(Entidad, Municipio, `Cve. Municipio`,`Bien jurídico afectado`, `Subtipo de delito`, total_sum, total_sex, hombres, mujeres, total) %>%
  mutate(tot_sex = sum(total_sex)) %>%
  ungroup() %>%
  select(Entidad, Municipio, `Cve. Municipio`, `Subtipo de delito`, total_sum, tot_sex, hombres, mujeres, total) 
  
# Filter out border cities
final_border <- final %>%
  filter(Entidad == 'Baja California' | Entidad == 'Sonora' | Entidad == 'Coahuila de Zaragoza' | Entidad == 'Nuevo León' |
           Entidad == 'Tamaulipas' | Entidad == 'Chihuahua') %>%
  filter(Municipio == 'Tijuana' |
           Municipio == 'Mexicali' |
           Municipio == 'San Luis Río Colorado' |
           Municipio == 'Nogales' |
           Municipio == 'Agua Prieta' |
           Municipio == 'Juárez' |
           Municipio == 'Acuña' |
           Municipio == 'Piedras Negras' |
           Municipio == 'Nuevo Laredo' |
           Municipio == 'Reynosa' |
           Municipio == 'Matamoros') %>%
  filter(`Cve. Municipio` != 8044 & `Cve. Municipio` != 19031 & `Cve. Municipio` != 5017 & `Cve. Municipio` != 5015) 

# Calculate per capita rates for the country for the four crimes
total_hom <- final %>%
  filter(`Subtipo de delito` == 'Homicidio doloso') %>%
  summarise(tot = sum(total_sum)) %>%
  mutate(tot_pc = (tot / total_pop) * 100000)

total_sec <- final %>%
  filter(`Subtipo de delito` == 'Secuestro') %>%
  summarise(tot = sum(total_sum)) %>%
  mutate(tot_pc = (tot / total_pop) * 100000)

total_fem <- final %>%
  filter(`Subtipo de delito` == 'Feminicidio') %>%
  summarise(tot = sum(total_sum)) %>%
  mutate(tot_pc = (tot / total_muj) * 100000)

total_sex <- final %>%
  filter(`Subtipo de delito` == 'Acoso sexual') %>%
  summarise(tot = sum(tot_sex)) %>%
  mutate(tot_pc = (tot / total_muj) * 100000)


# Calculate per capita rates for border cities for the four crimes
border_hom <- final_border %>%
  filter(`Subtipo de delito` == 'Homicidio doloso') %>%
  mutate(`Homicides per 100,000` = (total_sum / total) * 100000) %>%
  select(Entidad, Municipio, `Subtipo de delito`, `Homicides per 100,000`, total, total_sum) %>%
  unite(mun, Municipio, Entidad, sep = ', ') %>%
  arrange(`Homicides per 100,000`) %>%
  mutate(muni = factor(mun, levels = mun))

# Dividing by category of programs
type <- c("San Luis Río Colorado, Sonora", "Agua Prieta, Sonora", "Acuña, Coahuila de Zaragoza")
border_hom$category <- ifelse(border_hom$mun %in% type, "Metering", "MPP + Metering")

border_sec <- final_border %>%
  filter(`Subtipo de delito` == 'Secuestro') %>%
  mutate(pc = (total_sum / total) * 100000) %>%
  select(Entidad, Municipio, `Subtipo de delito`, pc, total, total_sum) %>%
  arrange(pc) %>%
  mutate(muni = factor(Municipio, levels = Municipio)) 

border_fem <- final_border %>%
  filter(`Subtipo de delito` == 'Feminicidio') %>%
  mutate(pc = (total_sum / mujeres) * 100000) %>%
  select(Entidad, Municipio, `Subtipo de delito`, pc, mujeres, total_sum) %>%
  arrange(pc) %>%
  mutate(muni = factor(Municipio, levels = Municipio))

border_sex <- final_border %>%
  filter(`Subtipo de delito` == 'Acoso sexual') %>%
  mutate(`Sex Crimes per 100,000` = (tot_sex / mujeres) * 100000) %>%
  select(Entidad, Municipio, `Subtipo de delito`, `Sex Crimes per 100,000`, mujeres, tot_sex) %>%
  unite(mun, Municipio, Entidad, sep = ', ') %>%
  arrange(`Sex Crimes per 100,000`) %>%
  mutate(muni = factor(mun, levels = mun)) 

border_sex$category <- ifelse(border_sex$mun %in% type, "Metering", "MPP + Metering")

# Calculate per capita for border cities
final_hom <- border_hom %>%
  summarise_at(c('total', 'total_sum'), sum) %>%
  mutate(pc_hom = (total_sum / total) * 100000)

final_kid <- border_sec %>%
  summarise_at(c('total', 'total_sum'), sum) %>%
  mutate(pc_kid = (total_sum / total) * 100000)

final_sex <- border_sex %>%
  summarise_at(c('mujeres', 'tot_sex'), sum) %>%
  mutate(pc_sex = (tot_sex / mujeres) * 100000)

final_fem <- border_fem %>%
  summarise_at(c('mujeres', 'total_sum'), sum) %>%
  mutate(pc_fem = (total_sum / mujeres) * 100000)

# Diverted lollipop for homicides
hom_plot <- ggplot(border_hom, aes(x=muni, y=`Homicides per 100,000`, color = category)) + 
  geom_point(stat='identity', size=2)  +
  geom_segment(aes(y = total_hom$tot_pc, 
                   x = muni, 
                   yend = `Homicides per 100,000`, 
                   xend = muni)) +
  scale_colour_manual(values = c(wola_grey, wola_blue)) +
  labs(title="Cities where large numbers of migrants are stranded, such as \nTijuana, Juárez, and Reynosa have homicide per capita rates \nthat far exceed the national rate", 
       subtitle="On the whole, border cities tend to have larger homicide per capita rates than \nthe national rate",
       caption='Sources: SESNSP municipal homicide data and CONAPO 2015 Census data') +
  geom_hline(yintercept = total_hom$tot_pc, linetype = 'dashed', color = wola_grey, show.legend = TRUE) +
  annotate('text', x = 2, y = 50, label = 'italic("Mexico homicides per 100,000 = 24.6")', size = 4, color = wola_grey, parse=TRUE) +
  coord_flip()

hom <- hom_plot + wola_theme 
hom
ggsave("Graphs/MX_Homicides.jpeg", width = 10, height = 6)

# Diverted lollipop for sexual crimes
sex_plot <- ggplot(border_sex, aes(x=muni, y=`Sex Crimes per 100,000`, color = category)) + 
  geom_point(stat='identity', size=2)  +
  geom_segment(aes(y = total_sex$tot_pc, 
                   x = muni, 
                   yend = `Sex Crimes per 100,000`, 
                   xend = muni)) +
  scale_colour_manual(values = c(wola_grey, wola_blue)) +
  labs(title="Border municipalities where large numbers of migrants are stranded \nreport more sex crimes per capita than the national rate", 
       subtitle="Of these, only Nuevo Laredo reports a slightly lower rate than national",
       caption='Sources: SESNSP municipal sex crime data and CONAPO 2015 Census data') +
  geom_hline(yintercept = total_sex$tot_pc, linetype = 'dashed', color = wola_grey, show.legend = TRUE) +
  annotate('text', x = 2, y = 110, label = 'italic("Mexico sex crimes per 100,000 = 75")', size = 4, color = wola_grey, parse=TRUE) +
  coord_flip()

sex <- sex_plot + wola_theme 
sex
ggsave("Graphs/MX_SexCrimes.jpeg", width = 10, height = 6)













