### Title: Chlorpleth

library(tidyverse)
library(here)
library(maps)
library(rworldmap)
library(viridis)
library(ggstance)

df_in <- read_csv("df_individuals.csv")
df_dic <- read_csv("dict.csv")
glimpse(df_in)
glimpse(df_dic)

## read in country codes seperately
regions_dic <- read_tsv(here("data/dic/geo.dic"), col_names = F)
colnames(regions_dic) <- c("code", "desc")


df_r <- df_in %>%
  left_join(df_dic, by = c("indic_is" = "code")) %>%
  left_join(df_dic, by = c("ind_type" = "code")) %>%
  left_join(df_dic, by = c("unit" = "code")) %>%
  left_join(regions_dic,
            by = c("geo.time" = "code")
            ) %>%
#  select(year,
#         measure = "details.x",
#         demograph = "details.y",
#         geo = "desc",
#         val
#         )
  rename(measure = details.x,
         demograph = details.y,
         geo = desc,
         unit_desc = details)


glimpse(df_r)
glimpse(df_dic)



unique(df_r$demograph)

## come back to this and add more later
demo_filt <- c("All Individuals",
               "Mobile Internet users",
               "ICT professionals",
               "Mobile internet users",
               "Non-users of mobile internet",
               "Non ICT professionals",
               "Individuals living in cities",
               "Individuals living in towns and suburbs",
               "Individuals living in rural areas"
               )

df_filt <- df_r %>%
  filter(demograph %in% demo_filt)

###try something else

theme_set(
  theme_void()
)

world_map <- map_data("world")

#the_cs <- countryExData %>%
#  filter(GEO_subregion %in% c("Central Europe", "Western Europe", "Eastern Europe")) %>%
#  select(Country) %>%
#  as_vector()


## Some EU Contries
the_cs <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
            "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
            "Croatia", "Cyprus", "Czech Republic",
            "Denmark",
            "Estonia",
            "Finland", "France",
            "Georgia", "Germany", "Greece",
            "Hungary",
            "Iceland", "Ireland", "Italy",
            "Kosovo",
            "Latvia", "Liechtenstein", "Lithuania", "Luxembourg",
            "Macedonia", "Malta", "Moldova", "Monaco", "Montenegro",
            "Netherlands", "Norway(?!:Svalbard)",
            "Poland", "Portugal",
            "Romania", "Russia",
            "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Switzerland", "Sweden",
            "Turkey", "Ukraine", "UK",
            "Vatican")

## Retrievethe map data
euro_maps <- map_data("world", region = the_cs)

#sort(unique(euro_maps$region))

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- euro_maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

#glimpse(euro_maps)

#min(euro_maps$long)
#max(euro_maps$long)
#min(euro_maps$lat)
#max(euro_maps$lat)

##join up

#unique(df_r$demograph)
#unique(df_r$measure)

df_for_j <-
  df_filt %>%
  mutate(geo = case_when(geo == "United Kingdom" ~ "UK",
                         geo == "North Macedonia" ~ "Macedonia",
                         geo == "Germany (until 1990 former territory of the FRG)" ~ "Germany",
                         geo == "Kosovo (under United Nations Security Council Resolution 1244/99)" ~ "Kosovo",
                         geo == "Czechia" ~ "Czech Republic",
                         TRUE ~ geo))

#now go into chloropleth
df_j <-
  df_for_j %>%
  filter(demograph == "All Individuals", measure == "Last online purchase: in the 12 months") %>%
  select(measure, unit_desc, demograph, geo, val) %>%
  right_join(euro_maps, by = c("geo" = "region"))

##right, lined up now

df_j %>%
  filter(unit_desc == "Percentage of individuals") %>%
ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = val), colour = "grey90")+
  ##geom_text(aes(label = geo), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_c(na.value = "grey80")+
  theme_void()+
##  theme(legend.position = "none") +
  coord_cartesian(x = c(-22, 38), y = c(36, 70))

## how much do they spend?
#unique(df_for_j$measure)

#df_for_j %>%
#  filter(demograph == "All Individuals",
#         str_detect(measure, "1000 euro"),
#         geo == "European Union - 28 countries (2013-2020)")


df_spending <-
df_for_j %>%
  filter(demograph == "All Individuals",
         str_detect(measure, "100 and 499|50 and 99|500 and 999|1000 euro|100 euro|less than 50"),
         geo == "European Union - 28 countries (2013-2020)",
         unit_desc == "Percentage of individuals who purchased online in the last 3 months") %>%
mutate(measure = str_remove_all(measure, c("Online purchases in the last 3 months for "))) %>%
mutate(measure = str_remove(measure, " \\(until 2019\\)")) %>%
mutate(measure = case_when(str_detect(measure, "50 euro") ~ "< 50",
                           str_detect(measure, "500 and 999") ~ "500 - 999",
                           str_detect(measure, "50 and 99") ~ "50 - 99",
                           str_detect(measure, "100 and 499") ~ "100 - 499",
                           str_detect(measure, "1000 euro") ~ "1000+",
                           str_detect(measure, "100 euro") ~ "100+"
                           )
       ) %>%
filter(measure != "100+") %>%
mutate(measure = fct_relevel(measure, c("< 50", "50 - 99", "100 - 499", "500 - 999", "1000+")))





#View(df_spending)

glimpse(df_spending)

df_spending %>%
  ggplot(aes(y = measure, x = val)) +
  geom_col() +
  theme_light()



df_spending %>%
  group_by(measure) %>%
  summarise(mean_val = mean(val, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(y = measure, x = mean_val)) +
  geom_col() +
  theme_light()

## Now for a dotplot of top online purchases
## with the countries as dots
df_top5_buys <-
  df_for_j %>%
  filter(demograph == "All Individuals",
         str_detect(measure, "Online purchases: "),
         !str_detect(geo, "Euro"),
         str_detect(unit_desc, "ordered good")) %>%
  filter(!str_detect(measure, "sellers"))

##just get top 5 by median
top5_meds <-
df_top5_buys %>%
  group_by(measure) %>%
  summarise(med_m = median(val, na.rm = T)) %>%
  ungroup() %>%
  arrange(desc(med_m))

top5_c <-
  top5_meds %>%
  select(measure) %>%
  as_vector()

df_top5_buys %>%
  filter(measure %in% top5_c[1:5]) %>%
  ggplot(aes(x = measure, y = val)) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               binwidth = 0.15,
               dotsize = 10
               ) +
  theme_bw()+
  coord_flip()

###JUNK=========================================


##try these
#library(ggmap)
#library(mapproj)

#register_google("AIzaSyA6cuW-WIgzb6S_WjEqDxEPdxzuJc7eifE")

#e_map <- get_map(location = "Europe", zoom = 4, source = "google", )


#glimpse(e_map)

#ggmap(e_map)
