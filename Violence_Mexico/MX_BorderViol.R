library(here)
library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)
library(reshape2)

# Read in data
pop <- read.csv(here("Violence_Mexico/Data/muni_pob.csv"), check.names = FALSE, skip = 4)
viol <- read.csv(here("Violence_Mexico/Data/Violence_data.csv"), check.names = FALSE)

per_capita <- viol %>%
  filter(`Subtipo de delito`=='Homicidio doloso') %>%
  summarise(sums = sum(Diciembre))

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




