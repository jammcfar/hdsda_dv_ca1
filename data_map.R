### Title: Chlorpleth

library(tidyverse)
library(here)

df_in <- read_csv("df_individuals.csv")
df_dic <- read_csv("dict.csv")
glimpse(df_in)
glimpse(df_dic)

## read in country codes seperately
regions_dic <- read_tsv(here("data/dic/geo.dic"), col_names = F)
colnames(regions_dic) <- c("code", "desc")


df_r <-
  df_in %>%
  left_join(df_dic, by = c("indic_is" = "code")) %>%
  left_join(df_dic, by = c("ind_type" = "code")) %>%
  left_join(regions_dic, by = c("geo.time" = "code")) %>%
  select(year, measure = "details.x", demograph = "details.y", geo = "desc", val)

###try something else
library(maps)
library(viridis)

theme_set(
  theme_void()
)

world_map <- map_data("world")
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

# Some EU Contries
some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy",
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic", "Ireland"
)
# Retrievethe map data
some.eu.maps <- map_data("world", region = some.eu.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(some.eu.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")

###JUNK=========================================


##try these
#library(ggmap)
#library(mapproj)

#register_google("AIzaSyA6cuW-WIgzb6S_WjEqDxEPdxzuJc7eifE")

#e_map <- get_map(location = "Europe", zoom = 4, source = "google", )


#glimpse(e_map)

#ggmap(e_map)
