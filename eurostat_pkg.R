## eurostat pacakge work

library(tidyverse)
library(eurostat)
library(knitr)
library(rvest)
library(ggridges)
library(ggrepel)
library(textshape)
library(here)
library(ggpubr)

##function to capitalise first letter
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


`%notin%` <- Negate(`%in%`)

##for colours
nord_frost <- c("#8FBCBB", "#88C0D0", "#81A1C1", "#5E81AC")
nord_aurora <- c("#BF616A","#D08770","#EBCB8B","#A3BE8C","#B48EAD")

nf7 <- colorRampPalette(nord_frost)(7)
nf <- colorRampPalette(nord_frost)(10)

## also SASHAs cols
sasha_cols <- function(n_cols) {
  c(
    '#e6194b',
    '#3cb44b',
    '#4363d8',
    '#f58231',
    '#911eb4',
    '#46f0f0',
    '#f032e6',
    '#bcf60c',
    '#fabebe',
    '#008080',
    '#e6beff',
    '#9a6324',
    '#fffac8',
    '#800000',
    '#aaffc3',
    '#808000',
    '#ffd8b1',
    '#000075',
    '#808080',
    '#ffffff',
    '#000000'
  )[1:n_cols]
}




# Get Eurostat data listing
toc <- get_eurostat_toc()

# Check the first items
kable(head(toc))

id <- search_eurostat("online")$code[2]
print(id)

##create table of what we want

chosen_sets <-
bind_rows(
  search_eurostat("Internet purchases")[1,],
  search_eurostat("online")[1,],
  search_eurostat("buying")[1,]
)

colnames(chosen_sets) <- str_to_sentence(colnames(chosen_sets))
chosen_sets <- chosen_sets %>% select(-Values)

cs_plot <- ggpubr::ggtexttable(x = chosen_sets)


ggsave(filename = "cs_plot.png",
       plot = cs_plot,
       height = 3,
       width = 33,
       units = "cm",
       dpi = 500)

#dat <- get_eurostat(id,
#                    time_format = "num",
#                    type = "label"
#                    )

##search ids for some things, taken from the bottom
id_problems <- search_eurostat("buying")$code[1]
id_orders <- search_eurostat("ordered")$code[1]
id_proc <- search_eurostat("online")$code[1]
id_purc <- search_eurostat("Internet purchases")$code[1]


#dat_purc <- get_eurostat(id_purc,
#                         time_format = "num",
#                         type = "label")
#

#dat_prob <- get_eurostat(id_problems,
#                       time_format = "num",
#                       type = "label")

##write these just to be safe
#write_csv(dat,paste0(getwd(), "/", "dat.csv")
#write_csv(dat_purc,paste0(getwd(), "/", "dat_purc.csv"))
#write_csv(dat_prob,paste0(getwd(), "/", "dat_prob.csv"))

dat_in <- read_csv(here("dat.csv"))
dat_purc_in <- read_csv(here("dat_purc.csv"))
dat_prob_in <- read_csv(here("dat_prob.csv"))


#limit to EU countries
c_omit <- c("Bosnia and Herzegovina",
            "Montenegro",
            "Kosovo (under United Nations Security Council Resolution 1244/99)",
            "North Macedonia",
            "Serbia",
            "Turkey",
            "Iceland",
            "Switzerland",
            "Norway")

dat <-
  dat_in %>%
  filter(geo %notin% c_omit)

dat_purc <-
  dat_purc_in %>%
  filter(geo %notin% c_omit)

dat_prob <-
  dat_prob_in %>%
  filter(geo %notin% c_omit)

##read in data_dict to get country codes
dat_dict <- read_csv(here("eu_dict.csv"))



dat <- dat %>%
  left_join(dat_dict, by = c("geo" = "description"))

dat_prob <- dat_prob %>%
  left_join(dat_dict, by = c("geo" = "description"))

dat_purc <- dat_purc %>%
  left_join(dat_dict, by = c("geo" = "description"))

##exploration
summary(dat_in)
#funModeling::status(dat_in)
#funModeling::status(dat_purc_in)
#funModeling::status(dat_prob_in)

funModeling::status(
    bind_rows(
        dat_in,
        dat_prob_in,
        dat_purc_in
    )
)

## Graph 1: line plot=============================================


dat_purc_lines <-
dat_purc %>%
filter(ind_type == "All Individuals",
       str_detect(indic_is, "Frequency of online"),
       str_detect(geo, "Euro area"),
       unit == "Percentage of individuals",
       !str_detect(indic_is, "6 times or more")) %>%
mutate(indic_is = str_remove(indic_is, "Frequency of online purchases in the last 3 months: ")) %>%
mutate(indic_is = str_replace(indic_is, "more", "More"))

plot_1 <-
ggplot(dat_purc_lines,
       aes(x = time,
           y = values,
           colour = indic_is,
           shape = indic_is)) +
  geom_path(alpha = 0.5) +
geom_point(size = 3) +
  theme_classic() +
  labs(title = "Growth of online purchasing over time",
       subtitle = "Lines represent data from the EU27/28",
       x = "Year", y = "% of individuals",
       colour = "Online purchasing\nin the last 3 months",
       shape = "Online purchasing\nin the last 3 months") +
scale_colour_manual(values = sasha_cols(4))

ggsave(filename = "plot1.png",
       plot = plot_1,
       height = 12,
       width = 16,
       units = "cm",
       dpi = 500)

## Graph 2: Chloropleth=======================================================

library(maps)
library(rworldmap)
library(viridis)

df_in <- read_csv("df_individuals.csv")
df_dic <- read_csv("dict.csv")

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
  rename(measure = details.x,
         demograph = details.y,
         geo = desc,
         unit_desc = details)

## come back to this and add more later
demo_filt <- c("All Individuals"
               )

df_filt <- df_r %>%
  filter(demograph %in% demo_filt) %>%
  left_join(dat_dict, by = c("geo" = "description"))


world_map <- map_data("world")

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
            #"Turkey",
            "Ukraine", "UK",
            "Vatican")

## Retrievethe map data
euro_maps <- map_data("world", region = the_cs)

#sort(unique(euro_maps$region))



# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- euro_maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))# %>%

df_for_j <-
  df_filt %>%
  mutate(geo = case_when(geo == "United Kingdom" ~ "UK",
                         geo == "North Macedonia" ~ "Macedonia",
                         geo == "Germany (until 1990 former territory of the FRG)" ~ "Germany",
                         geo == "Kosovo (under United Nations Security Council Resolution 1244/99)" ~ "Kosovo",
                         geo == "Czechia" ~ "Czech Republic",
                         TRUE ~ geo))

df_map_code <-
  df_for_j %>%
  select(geo, geo.time) %>%
  unique()

region.lab.data <-
  region.lab.data %>%
  left_join(df_map_code, by = c("region" = "geo"))

##now go into chloropleth
df_j <-
  df_for_j %>%
  filter(demograph == "All Individuals", measure == "Last online purchase: in the 12 months") %>%
  select(measure, unit_desc, demograph, geo, val, code) %>%
  right_join(euro_maps, by = c("geo" = "region"))

df_j$val[which(is.na(df_j$code))] <- NA

##right, lined up now

plot_2 <-
df_j %>%
  filter(unit_desc == "Percentage of individuals") %>%
ggplot() +
  geom_polygon(aes(fill = val, x = long, y = lat, group = group), colour = "grey90") +
  geom_text(aes(label = geo.time, x = long, y = lat), data = region.lab.data,  size = 3, hjust = 0.5) +
  #scale_fill_viridis_c(na.value = "grey80")+
  theme_void()+
##  theme(legend.position = "none") +
  coord_cartesian(x = c(-10, 30), y = c(36, 70)) +
  scale_fill_steps(na.value = "grey75", breaks = c(0,20,40,60,80,100))+
  labs(title = "Which countries buy online the most?",
       subtitle = "% of individuals who have purchased online in the last year (2019)",
       fill = "% of\nindividuals")
#scale_fill_viridis_b(na.value = "grey75", scale_color_steps())


ggsave(filename = "plot2.png",
       plot = plot_2,
       height = 12,
       width = 16,
       units = "cm",
       dpi = 500)



##graph 4: Heatmap================================================================


dat_purc_heat <-
dat_purc %>%
  filter(time == 2019,
         !str_detect(geo, "Euro|Canada"),
         ind_type == "All Individuals",
         unit == "Percentage of individuals",
         str_detect(indic_is, "Online purchases: "),
         !str_detect(indic_is, "sellers")) %>%
         mutate(indic_is = str_remove(indic_is, ".*: ")) %>%
  filter(!str_detect(indic_is, "other travel|others|or computer software|magazines/e-learn|software, del|music, delive|papers/e-l|software, delivered"),
         indic_is != "holiday accommodation"
         ) %>%
mutate(indic_is = firstup(indic_is))

##cluster the thing using ward.D2
#df_purc_heat_df <-
#dat_purc_heat %>%
#select(indic_is, code, values) %>%
#pivot_wider(names_from = code, values_from = values) %>%
#as.data.frame()

#rownames(df_purc_heat_df) <- df_purc_heat_df$indic_is
#df_purc_heat_df$indic_is <- NULL

#df_clust <- cluster_matrix(as.matrix(df_purc_heat_df), method = "ward.D")

##make tidy for ggplot
#df_c_tidy <-
#df_clust %>%
#as.data.frame()

#df_c_tidy$indic_is <- row.names(df_c_tidy)

#heat_test <-
#df_c_tidy %>%
#as_tibble() %>%
#mutate(indic_is = fct_inorder(indic_is)) %>%
#pivot_longer(cols = 1:28) %>%
#mutate(name = fct_inorder(name))


##more processing

##try fct_reorder
heat_test <-
dat_purc_heat %>%
  mutate(code = as_factor(code)) %>%
  mutate(code = fct_reorder(code, values, sum)) %>%
  mutate(indic_is = fct_reorder(indic_is, values, sum))


heat_test %>%
ggplot(aes(y = indic_is, x = code, size = values, colour = values)) +
  geom_point(shape = 15) +
  geom_text(aes(label = round(values, 2)), colour = "grey80") +
  theme_classic() +
  labs(title = "Which countries order different types of goods?",
       subtitle = "Text is % of individuals per EU country",
       caption = "Axes are ordered by total goods ordered",
       x = "Country code",
       y = "")

##heat_test %>%
##ggplot(aes(y = indic_is, x = name, size = value, colour = values)) +
##geom_point(shape = 15)

unique(heat_test$name)

##df_c_tidier <-
 ## df_c_tidy %>%
  ##filter(!str_detect(indic_is, "other travel|others|or computer software|magazines/e-learn|software, del|music, delive|papers/e-l|software, delivered"),
   ##      indic_is %notin% c(" holiday accomodation"))

##dat_purc_heat %>%
##  ggplot(aes(x = geo, y = indic_is, size = values, colour = values)) +
##  geom_point() +
##  theme_classic()


##  Graph 5:buying strategies======================================================================
dat_strategies <-
dat %>%
  filter(str_detect(unit, "who ordered goods"),
         !str_detect(geo, "Euro"),
         ind_type == "All Individuals") %>%
  mutate(buy_type = case_when(str_detect(indic_is, "customer reviews") ~ "Customer reviews",
                              str_detect(indic_is, "product comparison") ~ "Product comparison sites",
                              str_detect(indic_is, "advertisement") ~ "Advertisements",
                              str_detect(indic_is, "several retailer") ~ "Comparing sites manually"),
         usage = case_when(str_detect(indic_is, "never") ~ "Never",
                           str_detect(indic_is, "some times") ~ "Sometimes",
                           str_detect(indic_is, "every time") ~ "Always",
                           str_detect(indic_is, "did not buy") ~ "No",
                           str_detect(indic_is, "bought/ordered online") ~ "Yes")
         ) %>%
mutate(usage = fct_relevel(usage, "Never", "Sometimes", "Always")) %>%
mutate(buy_type = fct_relevel(buy_type, "Customer reviews", "Product comparison sites", "Comparing sites manually"))


dat_strategies %>%
  ggplot(aes(y = buy_type, x = values, fill = usage)) +
  geom_density_ridges(alpha = 0.5) +
  geom_point()

##graph
dat_strategies %>%
  filter(buy_type != "Advertisements") %>%
  ggplot(aes(x = buy_type, y = values, fill = usage)) +
  geom_violin(alpha =0.6) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               position = "dodge",
               binwidth = 0.8) +
  stat_summary(aes(group = usage),
               fun.y = "mean",
               position = position_dodge(0.9),
               geom = "crossbar") +
  theme_classic() +
  scale_fill_manual(values = nord_aurora[c(1,3,5)]) +
  labs(x = "",
       y = "% of individuals",
       title = "Comparing online buying strategies (2019)",
       subtitle = "Dots represent indivdual european countries, bar represents the average",
       fill = "How often do\nthey use these\nmethods?")




#Graph 6: ``#try a histogram of advertisements====================================================

dat_ads <-
dat %>%
  filter(str_detect(unit, "who ordered goods"),
         !str_detect(geo, "Euro"),
         str_detect(ind_type, "Individuals, ")) %>%
  mutate(buy_type = case_when(str_detect(indic_is, "customer reviews") ~ "Customer reviews",
                              str_detect(indic_is, "product comparison") ~ "Product comparison sites",
                              str_detect(indic_is, "advertisement") ~ "Advertisements",
                              str_detect(indic_is, "several retailer") ~ "Comparing sites manually"),
         usage = case_when(str_detect(indic_is, "never") ~ "Never",
                           str_detect(indic_is, "some times") ~ "Sometimes",
                           str_detect(indic_is, "every time") ~ "Always",
                           str_detect(indic_is, "did not buy") ~ "No",
                           str_detect(indic_is, "bought/ordered online") ~ "Yes")
         ) %>%
  mutate(usage = fct_relevel(usage, "Never", "Sometimes", "Always")) %>%
  filter(buy_type == "Advertisements")

dat_ads_hist <-
dat_ads %>%
  filter(usage == "Yes",
         !str_detect(ind_type, "15 years|25 to 54|25 to 64|55 to 74")) %>%
mutate(ind_type = str_remove(ind_type, "Individuals, "))

## top 3 per group
dat_ads_top3 <-
dat_ads_hist %>%
  group_by(ind_type) %>%
  arrange(desc(values)) %>%
top_n(1, values) %>%
ungroup() %>%
filter(geo != "Spain")

dat_ads_means <-
  dat_ads_hist %>%
  group_by(ind_type) %>%
  summarise(mean_val = median(values, na.rm = T))


ggplot(dat_ads_hist, aes(x = values, y = ind_type, fill = ind_type)) +
  geom_density_ridges(stat = "binline", alpha = 0.8, scale = 1) +
  geom_text_repel(data = dat_ads_top3,
                  aes(label = geo),
                  nudge_y = -0.2,
                  nudge_x = 0,
                  segment.colour = NA) +
  geom_point(data = dat_ads_means, aes(x = mean_val, y = ind_type)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_manual(values = nf7) +
  labs(title = "Is buying online different between age groups?",
       subtitle = "Bars made up of European countries, black dot is the median",
         x = "Percentage of individuals with purchases in the past year",
       y = "Age group")


## Scatter plots?================================================================

dat_purc_time <-
dat_purc %>%
  filter(unit == "Percentage of individuals",
         ind_type == "All Individuals",
         str_detect(indic_is, ": from sellers|from national"),
         str_detect(geo, "28 countries")) %>%
filter(!str_detect(indic_is, "from sellers abroad")) %>%
mutate(indic_is = str_remove(indic_is, "Online purchases: "))

unique(dat_prob$ind_type)

#a plot of purchasing locations over time MAYBE USE LATER
dat_purc_time %>%
  ggplot(aes(x = time, y = values, colour = indic_is, shape = indic_is)) +
  geom_path() +
  geom_point(size = 5) +
  theme_classic() +
  scale_y_continuous(breaks = 2008:2019) +
  labs(title = "The growth of online purchases over time",
       subtitle = "Lines represent data from the EU 27/28",
       x = "Year", y = "% of individuals",
       colour = "Location of sellers",
       shape = "Location of sellers"
       )

##source and problems scatter
dat_prob_2019 <-
dat_prob %>%
  filter(unit == "Percentage of individuals",
         ind_type == "All Individuals",
         str_detect(indic_is, "encountered problems"),
         !str_detect(geo, "Euro"),
         time == 2019)

dat_purc_2019 <-
dat_purc %>%
   filter(unit == "Percentage of individuals",
         ind_type == "All Individuals",
         str_detect(indic_is, ": from sellers|from national"),
         !str_detect(geo, "Euro"),
         time == 2019) %>%
filter(!str_detect(indic_is, "from sellers abroad"))

dat_purc_2019 %>%
  left_join(dat_prob_2019, by = c("geo" = "geo")) %>%
  ggplot(aes(x = values.x, y = values.y, shape = indic_is.x, colour = indic_is.x)) +
  geom_point() +
  geom_smooth(method = "lm")
  theme_classic() +
  labs(x = "Online purchases", y = "Problems")

# try a different one. Try a bubble one.
unique(dat_purc$ind_type)
glimpse(dat_purc_2019)





##try a correlation matrix for problems

dat_allprobs <-
  dat_prob %>%
    filter(unit == "Percentage of individuals",
         ind_type == "All Individuals",
         str_detect(indic_is, "encountered the following"),
         !str_detect(geo, "Euro"),
         time == 2019) %>%
  mutate(indic_is = str_remove(indic_is, ".*:"))

prob_cors <-
dat_purc_2019 %>%
left_join(dat_allprobs, by = c("geo" = "geo")) %>%
  select(sellers = "indic_is.x",
         problems = "indic_is.y",
         geo,
         pur_val = "values.x",
         prob_val = "values.y") %>%
mutate(sellers = case_when(str_detect(sellers, "national") ~ "National",
                           str_detect(sellers, " EU") ~ "EU",
                           str_detect(sellers, "world") ~ "Rest of world",
                           str_detect(sellers, "unknown") ~ "Unknown origin")) %>%
  group_by(sellers, problems) %>%
summarise(cors = cor(pur_val, prob_val)) +
filter(sellers != "National" & !str_detect(problems, "foreign retailer")) #doesnt work?


##this would probably doe better as a stacked bar chart
##Use problems, and what is purchased... Better use...
ggplot(prob_cors, aes(x = sellers,
                      y = problems,
                      size = cors,
                      colour = cors,
                      fill = cors)
       ) +
  geom_point(shape = 22) +
  geom_text(aes(label = round(cors, 2)), size = 3, colour = "grey70") +
  theme_classic() +
  scale_size_continuous(range = c(1, 20)) +
  scale_fill_gradientn(colours = nf7) +
  scale_colour_gradientn(colours = nf7)


##try the stacked barplot
ggplot(prob_cors, aes(x = sellers,
                      y = cors,
                      fill = problems))


#actually, probably need to use cors?
dat_purc_4bars <-
dat_purc %>%
   filter(unit == "Percentage of individuals",
         ind_type == "All Individuals",
         str_detect(indic_is, ": from sellers|from national"),
         geo == "European Union - 28 countries (2013-2020)",
         time == 2019) %>%
filter(!str_detect(indic_is, "from sellers abroad"))
unique(dat_prob$ind_type)

dat_allprobs <-
  dat_prob %>%
    filter(unit == "Percentage of individuals",
         ind_type == "All Individuals",
         str_detect(indic_is, "encountered the following"),
         geo == "European Union - 28 countries (2013-2020)",
         time == 2019) %>%
  mutate(indic_is = str_remove(indic_is, ".*:")) %>%
left_join(dat_purc_4bars, by = c("geo" = "geo")) %>%
  select(sellers = "indic_is.x",
         problems = "indic_is.y",
         geo,
         pur_val = "values.x",
         prob_val = "values.y") %>%
mutate(sellers = case_when(str_detect(sellers, "national") ~ "National",
                           str_detect(sellers, " EU") ~ "EU",
                           str_detect(sellers, "world") ~ "Rest of world",
                           str_detect(sellers, "unknown") ~ "Unknown origin"))


dat_allprobs
ggplot(dat_allprobs, aes(x = sellers, y = prob_val, fill = problems)) +
  geom_col()

##Area do source data=============================================================

kable(search_eurostat("buying"))
kable(search_eurostat("ordered"))
kable(search_eurostat("online"))
kable(search_eurostat("internet"))
kable(search_eurostat("Internet purchases"))


dat_proc <- get_eurostat(id_proc,
                         time_format = "num",
                         type = "label")

dat_orders <- get_eurostat(id_orders,
                           time_format = "num")

dat_buy <- get_eurostat(id_barriers,
                       time_format = "num",
                       type = "label")


dat_prob
unique(dat_prob$indic_is)

dat_purc

unique(dat_purc$indic_is)
unique(dat_purc$ind_type)
unique(dat_purc$unit)
unique(dat_purc$time)
unique(dat_purc$geo)


dat_orders <- label_eurostat(dat_orders, fix_duplicated = T)
dat_orders
unique(dat_orders$indic_is)
unique(dat_orders$ind_type)
unique(dat_order$unit)


unique(dat_buy$indic_is)
unique(dat_buy$ind_type)
unique(dat_buy$unit)


dat_buy
