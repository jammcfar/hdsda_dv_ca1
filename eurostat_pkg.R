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
library(patchwork)
##library(extrafont)

##fonts()

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


#dat <- get_eurostat(id,
#                    time_format = "num",
#                    type = "label"
#                    )

##search ids for some things, taken from the bottom
id_problems <- search_eurostat("buying")$code[1]
id_orders <- search_eurostat("ordered")$code[1]
id_proc <- search_eurostat("online")$code[1]
id_purc <- search_eurostat("Internet purchases")$code[1]

barriers_lines <- toc[grep("isoc_ec_inb", toc$code),]
barriers_lines$code
barriers_id1 <- barriers_lines$code[1]


dat_orders2 <-
    get_eurostat(barriers_id1,
                 time_format = "num",
                 type = "label")


#dat_orders2 <- label_eurostat(dat_orders2, fix_duplicated = T)
#
#
#dat_orders <- get_eurostat(id_orders,
#                           time_format = "num")
#
#dat_orders <- label_eurostat(dat_orders, fix_duplicated = T)
#unique(dat_orders$indic_is)
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
#write_csv(dat_orders2, paste0(getwd(), "/", "dat_prob2.csv"))

dat_in <- read_csv(here("dat.csv"))
dat_purc_in <- read_csv(here("dat_purc.csv"))
dat_prob_in <- read_csv(here("dat_prob.csv"))
dat_barr_in <- read_csv(here("dat_prob2.csv"))

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

dat_barr <-
  dat_barr_in %>%
  filter(geo %notin% c_omit)

##read in data_dict to get country codes
dat_dict <- read_csv(here("eu_dict.csv"))



dat <- dat %>%
  left_join(dat_dict, by = c("geo" = "description"))

dat_prob <- dat_prob %>%
  left_join(dat_dict, by = c("geo" = "description"))

dat_purc <- dat_purc %>%
  left_join(dat_dict, by = c("geo" = "description"))

dat_barr <- dat_barr %>%
  left_join(dat_dict, by = c("geo" = "description"))

##exploration


##create table of what we want=========================================

chosen_sets <-
bind_rows(
  search_eurostat("Internet purchases")[1,],
  search_eurostat("online")[1,],
  search_eurostat("buying")[1,]
)

colnames(chosen_sets) <- str_to_sentence(colnames(chosen_sets))
chosen_sets <- chosen_sets %>% select(-Values)

chosen_sets$`Last table structure change` <- NULL
chosen_sets$Type <- NULL

colnames(chosen_sets)[3] <- "Last updated"


row_col <- c(nrow(dat_in), nrow(dat_prob_in), nrow(dat_purc_in))

chosen_sets <-
    chosen_sets %>%
    add_column("Rows" = row_col)

##

cs_plot <- ggpubr::ggtexttable(x = chosen_sets)


ggsave(filename = "cs_plot.png",
       plot = cs_plot,
       height = 3,
       width = 33,
       units = "cm",
       dpi = 500)

combo_dat_stats <-
    funModeling::status(
        bind_rows(
            dat_in,
            dat_prob_in,
            dat_purc_in
        )
        )

combo_dat_stats$q_zeros <- NULL
combo_dat_stats$q_na <- NULL
combo_dat_stats$q_inf <- NULL

combo_dat_stats <-
  combo_dat_stats %>%
  mutate(p_zeros = round(p_zeros*100, 2),
         p_na = round(p_na*100, 2)) %>%
  rename(Variable = variable,
         Type = type,
         `N unique` = unique,
         `% zeros` = p_zeros,
         `% NAs` = p_na,
         `% Inf` = p_inf,
         ) %>%
  select(1, 5, 6, 2, 3, 4)

cd_plot <- ggpubr::ggtexttable(x = combo_dat_stats)


ggsave(filename = "cd_plot.png",
       plot = cd_plot,
       height = 5.75,
       width = 18,
       units = "cm",
       dpi = 500)

##get metrics where missing
#combo_dat_nas <- bind_rows(dat,dat_purc,dat_prob) %>% filter(is.na(values))

#dat_nas <- naniar::as_shadow(combo_dat_nas)

#nas_rank <-
#combo_dat_nas %>%
#  group_by(indic_is) %>%
#  naniar::miss_var_summary() %>%
#  arrange(desc(pct_miss))


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
          ## colour = indic_is,
           shape = indic_is)) +
  geom_path(aes(linetype = indic_is),
            alpha = 0.5,
            colour = "#1E448A") +
geom_point(size = 3, colour  ="#1E448A") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(colour = "grey90")) +
  labs(title = "Growth of online purchasing over time",
       subtitle = "Lines represent data from the EU27/28",
       x = "Year", y = "% of individuals",
       colour = "Online purchasing\nin the last 3 months",
       shape = "Online purchasing\nin the last 3 months") +
  scale_linetype_manual(values = c("solid", "longdash", "dashed", "dotted")) +
  scale_y_continuous(breaks = c(0, 4, 8, 12, 16, 20)) +
  coord_cartesian(ylim = c(0,20)) +
  guides(linetype = FALSE)
##scale_colour_manual(values = sasha_cols(4))

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

df_r %>%
  filter(str_detect(measure, "haven't ordered"),
         !is.na(val)) %>%
  select(indic_is, measure) %>%
  unique()

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
  left_join(df_map_code, by = c("region" = "geo")) %>%
  filter(geo.time %notin% c("CH", "NO", "BA", "ME", "AL", "XK", "MK", "RS"))

##now go into chloropleth
df_j <-
  df_for_j %>%
  filter(demograph == "All Individuals", measure == "Last online purchase: in the 12 months") %>%
  select(measure, unit_desc, demograph, geo, val, code) %>%
  right_join(euro_maps, by = c("geo" = "region"))

df_j$val[which(is.na(df_j$code))] <- NA

##right, lined up now

df_j$val_c <- cut(df_j$val, breaks = c(seq(0, 100, by = 20)))

plot_2 <-
df_j %>%
  filter(unit_desc == "Percentage of individuals") %>%
ggplot() +
  geom_polygon(aes(fill = val_c, x = long, y = lat, group = group), colour = "grey80", size = 0.5) +
  geom_text(aes(label = geo.time, x = long, y = lat), data = region.lab.data,  size = 3, hjust = 0.5, colour = "#FDCB0B") +
  #scale_fill_viridis_c(na.value = "grey80")+
  theme_void()+
##  theme(legend.position = "none") +
coord_cartesian(x = c(-10, 30), y = c(36, 70)) +
scale_fill_manual(drop = F,
                  na.value = "grey75",
                  values = c("#93CCC6","#75B6BC", "#5795AC", "#3A6F9B","#1E448A"   )
                  ) +
#scale_fill_stepsn(na.value = "grey75",
#                  colours = c("#93CCC6","#75B6BC", "#5795AC", "#3A6F9B","#1E448A"   ),
 #
  #               breaks = c(0,20,40,60,80,100))+
  labs(title = "How do EU28 states differ in online purchasing?",
       subtitle = "% of individuals who have purchased online in the last year (2019)",
       fill = "% of\nindividuals")# +
guides(fill = guide_legend(guide_legend(
         title = "% of\nindividuals",
         keyheight = unit(3, units = "mm"),
                                        keywidth=unit(12, units = "mm"),
                                        label.position = "bottom",
         title.position = 'top', nrow=1)))
#scale_fill_viridis_b(na.value = "grey75", scale_color_steps())

ggsave(filename = "plot2.png",
       plot = plot_2,
       height = 12,
       width = 16,
       units = "cm",
       dpi = 500)


##graph 3: Stacked bar============================================================

dat_stack <-
  dat_purc %>%
    filter(ind_type == "All Individuals",
        str_detect(indic_is, "3 months for"),
        !str_detect(geo, "Euro"),
        str_detect(unit, "online in the last 3 months")) %>%
 ## mutate(geo = fct_reorder(geo, values, sum)) %>%
 mutate(indic_is = case_when(str_detect(indic_is , "50 euro") ~ "< 50",
                             str_detect(indic_is, "50 and 99") ~ "50 - 99",
                             str_detect(indic_is, "100 and 499") ~ "100 - 499",
                             str_detect(indic_is, "500 and 999") ~ "500 - 999",
                             str_detect(indic_is, "1000 euro or more") ~ "1000 or more")) %>%
 filter(!is.na(indic_is)) %>%
 mutate(indic_is = factor(indic_is, levels = c("< 50", "50 - 99", "100 - 499", "500 - 999", "1000 or more"))) %>%
 mutate(geo = fct_reorder(geo, values * (0 + 100000 * (indic_is == "1000 or more")), max))

ggplot(dat_stack, aes(x = geo, y = values, fill = indic_is)) +
  geom_col()

##forget that, do a pretty bar

dat_bar <-
  dat_purc %>%
    filter(ind_type == "All Individuals",
        str_detect(indic_is, "3 months for"),
        str_detect(geo, "28 count"),
        str_detect(unit, "online in the last 3 months"),
        time == 2019) %>%
 mutate(indic_is = case_when(str_detect(indic_is , "50 euro") ~ "< 50",
                             str_detect(indic_is, "50 and 99") ~ "50 - 99",
                             str_detect(indic_is, "100 and 499") ~ "100 - 499",
                             str_detect(indic_is, "500 and 999") ~ "500 - 999",
                             str_detect(indic_is, "1000 euro or more") ~ "1000 or more")) %>%
 filter(!is.na(indic_is)) %>%
 mutate(indic_is = factor(indic_is, levels = c("< 50", "50 - 99", "100 - 499", "500 - 999", "1000 or more")))

plot3 <-
ggplot(dat_bar, aes(y = indic_is, x = values)) +
  geom_col(colour = "#1E448A", fill = "#1E448A") +
  geom_text(aes(label = paste0(values, "%"),
                x = values,
                y = indic_is,
                hjust = 1.25),
            colour = "#FDCB0B") +
  annotate("text", x = 11, y = 5,
           label = "45% of these are in\nthe highest income quartile",
           colour = "grey50",
           hjust = 0, size = 3) +
  annotate("text", x = 17, y = 1,
           label = "46% of these are in\nthe lowest income quartile",
           colour = "grey50",
           hjust = 0, size = 3) +
  coord_cartesian(xlim = c(0, 60)) +
  labs(title = "Money spent online",
       subtitle = "Among individuals who have purchased goods online in the last 3 months",
       x = "", y = "Euros") +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())

ggsave(filename = "plot3.png",
       plot = plot3,
       height = 12,
       width = 16,
       units = "cm",
       dpi = 500)

##check up on mobile/non mobile users
money_check <-
  dat_purc %>%
    filter(str_detect(ind_type, "quartile"),
        str_detect(indic_is, "3 months for"),
        str_detect(geo, "28 count"),
        str_detect(unit, "online in the last 3 months"),
        time == 2019) %>%
 mutate(indic_is = case_when(str_detect(indic_is , "50 euro") ~ "< 50",
                             str_detect(indic_is, "50 and 99") ~ "50 - 99",
                             str_detect(indic_is, "100 and 499") ~ "100 - 499",
                             str_detect(indic_is, "500 and 999") ~ "500 - 999",
                             str_detect(indic_is, "1000 euro or more") ~ "1000 or more")) %>%
 filter(!is.na(indic_is)) %>%
 mutate(indic_is = factor(indic_is, levels = c("< 50", "50 - 99", "100 - 499", "500 - 999", "1000 or more")))

##graph 4: Heatmap================================================================


dat_purc_heat <-
dat_purc %>%
  filter(time == 2019,
         !str_detect(geo, "Euro|Canada"),
         ind_type == "All Individuals",
         unit == "Percentage of individuals who ordered goods or services, over the internet, for private use, in the last year",
         str_detect(indic_is, "Online purchases: "),
         !str_detect(indic_is, "sellers")) %>%
         mutate(indic_is = str_remove(indic_is, ".*: ")) %>%
  filter(!str_detect(indic_is, "other travel|others|or computer software|magazines/e-learn|software, del|music, delive|papers/e-l|software, delivered"),
         indic_is != "holiday accommodation"
         ) %>%
mutate(indic_is = firstup(indic_is)) %>%
mutate(indic_is = case_when(indic_is == "Medecine" ~ "Medicine",
                            indic_is == "Travel and holiday accommodation" ~ "Holidays",
                            indic_is == "Books/magazines/newspapers" ~ "Print media",
                            TRUE ~ indic_is))

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
  mutate(code = fct_reorder(code, values, median)) %>%
  mutate(indic_is = fct_reorder(indic_is, values, median))

plot_4 <-
heat_test %>%
ggplot(aes(y = indic_is, x = code, fill = values)) +
## geom_point(shape = 15) +
geom_raster() +
  geom_text(aes(label = round(values, 2)), colour = "grey80") +
theme_classic() +
theme(legend.position = "none") +
  labs(title = "Which countries order different types of goods?",
       subtitle = "Text is % of individuals per EU country",
       caption = "Axes are ordered by total goods ordered",
       x = "Country code",
       y = "")


ggsave(filename = "plot4.png",
       plot = plot_4,
       height = 12,
       width = 16,
       units = "cm",
       dpi = 500)

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

##new one; a bar chart with the
library(ggflags)

dat_flags <-
dat_purc_heat %>%
  mutate(code = str_to_lower(code)) %>%
  group_by(indic_is) %>%
    arrange(desc(values)) %>%
    filter(row_number() == 1:5) %>%
    ##slice_head(values, 5) %>%
    mutate(c_rank = row_number()) %>%
    top_n(indic_is, 5) %>%
    filter(str_detect(indic_is, "sports|Holidays|events|House|Electronic")) %>%
mutate(indic_is = fct_reorder(indic_is, values, sum)) %>%
##mutate(indic_is = factor(indic_is = c("Print media",
 ##                                     ""
  ##                         "Clothes, sports goods",
##""))
mutate(indic_is = fct_rev(indic_is)) %>%
arrange(indic_is) %>%
mutate(ind_num = as.integer(indic_is)) %>%
mutate(code = case_when(code == "uk" ~ "gb",
                        code == "ne" ~ "nl",
                        code == "dk" ~ "dk",
                        code == "fi" ~ "fi",
                        code == "de" ~ "de",
                        code == "ee" ~ "ee",
                        code == "se" ~ "se",
                        code == "ie" ~ "ie",
                        code == "ro" ~ "ro",
                        code == "cz" ~ "cz",
                        code == "at" ~ "at",
                        code == "bg" ~ "bg",
                        code == "hu" ~ "hu"))

data.frame(dat_flags$geo, dat_flags$code)

plot4_pre <-
  dat_flags %>%
  mutate(indic_is = fct_relevel(indic_is, "Electronic equipment", after = 0)) %>%
  mutate(indic_is = fct_relevel(indic_is, "Household goods", after = 1)) %>%
 # mutate(values = (22/7)*(values^2)) %>%
  mutate(values = (values/3.14)^0.5) %>%
  ggplot(aes(country)) +
  geom_flag(aes(x = indic_is, y = c_rank, country = code, size = values)) +
  geom_text(data = dat_flags,
            aes(x = indic_is, y = c_rank, label = paste0(values, "%")),
            vjust = 5,
            size = 1.25,
            colour = "grey50") +
 ## facet_wrap(~ indic_is) +
# scale_size(range = c(2.006, 4.484), guide = F) +

scale_size(range = c(3.29, 4.92), guide = F) +
coord_cartesian(ylim = c(-1, 6)) +
coord_flip() +
theme_classic() +
theme(axis.line = element_blank(),
      axis.ticks = element_blank(),
      text = element_text(size = 5)) +
labs(title = "Top five most popular types of purchases",
     subtitle = "Using data from individuals with online purchases\n in the last 3 months",
  x = "",
     y = "Top 5 states (from left to right)",
     size = "% of individuals\nwith purchases\nin last 3 months")
#probably close enough to area
ggsave(filename = "plot4_pre.png",
       plot = plot4_pre,
       height = 8,
       width = 7,
       units = "cm",
       dpi = 500)

ggsave(filename = "plot4_pre.png",
       plot = plot4_pre,
       height = 8,
       width = 7,
       units = "cm",
       dpi = 500)



set.seed(1234)
d <- data.frame(x=rnorm(50), y=rnorm(50),
                country=sample(c("ar","fr", "nz", "gb", "es", "ca"), 50, TRUE),
                stringsAsFactors = FALSE)
ggplot(d, aes(x=x, y=y, country=country, size=x)) +
  geom_flag() +
  scale_country() +
  scale_size(range = c(2, 15))



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

## a small df to label top and bottom
strats_top <-
  bind_rows(
    dat_strategies %>%
        filter(buy_type != "Advertisements") %>%
        filter(usage == "Always") %>%
        group_by(usage, buy_type) %>%
        top_n(1, values),

    dat_strategies %>%
        filter(buy_type != "Advertisements") %>%
        filter(usage == "Never") %>%
        group_by(usage, buy_type) %>%
    top_n(1, values)
    )



##graph
plot5 <-
dat_strategies %>%
filter(buy_type != "Advertisements") %>%
##filter(usage != "Never") %>%
  ggplot(aes(x = buy_type, y = values, fill = usage)) +
  geom_violin(alpha =0.7) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               position = "dodge",
               binwidth = 0.8) +
  stat_summary(aes(group = usage),
               fun.y = "mean",
               position = position_dodge(0.9),
               geom = "crossbar") +
scale_fill_manual(values = c("#8A4B1E", "#8A811E", "#1E448A")) +
#scale_colour_manual(values = c("#8A4B1E", "#8A811E", "#1E448A")) +
facet_wrap(buy_type ~ ., scales = "free_x") +
ggrepel::geom_text_repel(data = strats_top,
                         aes(label = geo),
                         arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "first")) +
theme_classic() +
theme(axis.line.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()) +
  labs(x = "",
       y = "% of individuals",
       title = "Comparing online buying strategies (2019)",
       subtitle = "Dots represent indivdual european countries, bar represents the average",
       fill = "How often do\nthey use these\nmethods?")


ggsave(filename = "plot5_pre.png",
       plot = plot5,
       height = 12,
       width = 24,
       units = "cm",
       dpi = 500)


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

plot6_age <-
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
  scale_fill_manual(values =
c("#1E448A",
"#3A6F9B",
"#5795AC",
"#75B6BC",
"#93CCC6",
"#B1DBCF",
"#D0EADE")) +
  labs(##title = "Is buying online different between age groups?",
       ##subtitle = "Bars made up of European countries, black dot is the median",
    x = "% of individuals with purchases in the past year",
    y = "")

##make a males/females one

dat_gend <-
dat %>%
  filter(str_detect(unit, "who ordered goods"),
         !str_detect(geo, "Euro"),
         str_detect(ind_type, "Males, 16 to 74|Females, 16 to 74")) %>%
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
  filter(buy_type == "Advertisements") %>%
filter(usage == "Yes") %>%
mutate(ind_type = case_when(str_detect(ind_type, "Males") ~ "Males",
                            str_detect(ind_type, "Females") ~ "Females"))

dat_gend_means <-
  dat_gend %>%
  group_by(ind_type) %>%
  summarise(mean_val = mean(values, na.rm = T))


dat_gend_top3 <-
dat_gend %>%
  group_by(ind_type) %>%
  arrange(desc(values)) %>%
top_n(1, values) %>%
ungroup()

plot_gend <-
ggplot(dat_gend, aes(x = values, y = ind_type, fill = ind_type)) +
  geom_density_ridges(stat = "binline", alpha = 0.8, scale = 1) +
  geom_text_repel(data = dat_gend_top3,
                  aes(label = geo),
                  nudge_y = -0.2,
                  nudge_x = 0,
                  segment.colour = NA) +
  geom_point(data = dat_gend_means, aes(x = mean_val, y = ind_type)) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(##title = "Is buying online different between genders?",
       ##subtitle = "Bars made up of European countries, black dot is the median",
         x = "% of individuals with purchases in the past year",
       y = "") +
  scale_fill_manual(values = c("#8A4B1E", "#1E448A"))

plot6 <-
plot6_age + plot_gend +plot_annotation(
  title = 'Does online buying vary with age and gender?',
  subtitle = 'Bars made up of states in the EU28, black dot is the median'
)


##fuck the above, its based on ADs!===================================
dat_gend <-
dat_purc %>%
  filter(str_detect(unit, "Percentage of individuals"),
         !str_detect(geo, "Euro"),
         str_detect(ind_type, "Males, 16 to 74|Females, 16 to 74"),
         indic_is == "Last online purchase: in the last 3 months",
         time == 2019) %>%
mutate(ind_type = case_when(str_detect(ind_type, "Males") ~ "Males",
                            str_detect(ind_type, "Females") ~ "Females"))

dat_gend_means <-
  dat_gend %>%
  group_by(ind_type) %>%
  summarise(mean_val = median(values, na.rm = T))

dat_gend_top3 <-
dat_gend %>%
  group_by(ind_type) %>%
  arrange(desc(values)) %>%
top_n(1, values) %>%
ungroup()


plot_gend <-
ggplot(dat_gend, aes(x = values, y = ind_type, fill = ind_type)) +
  geom_density_ridges(stat = "binline", alpha = 0.8, scale = 1) +
#  geom_text_repel(data = dat_gend_top3,
#                  aes(label = geo),
#                  nudge_y = -0.2,
#                  nudge_x = 0,
#                  segment.colour = NA) +
  geom_point(data = dat_gend_means, aes(x = mean_val, y = ind_type), size = 2) +
  theme_classic() +
  theme(legend.position = "none") +
  labs(##title = "Is buying online different between genders?",
       ##subtitle = "Bars made up of European countries, black dot is the median",
    x = "% with online purchases in the last 3 months",
       y = "") +
  scale_fill_manual(values = c("#8A4B1E", "#1E448A"))


#now age!
dat_age <-
dat_purc %>%
  filter(str_detect(unit, "Percentage of individuals"),
         !str_detect(geo, "Euro"),
         str_detect(ind_type, "Individuals, "),
         indic_is == "Last online purchase: in the last 3 months",
         time == 2019) %>%
filter(!str_detect(ind_type, "15 years|25 to 54|25 to 64|55 to 74")) %>%
filter(!str_detect(ind_type, "16 to 19|16 to 29|20 to 24|25 to 29")) %>%
mutate(ind_type = str_remove(ind_type, "Individuals, "))

dat_age_means <-
  dat_age %>%
  group_by(ind_type) %>%
  summarise(mean_val = mean(values, na.rm = T))

dat_age_top3 <-
dat_age %>%
  group_by(ind_type) %>%
  arrange(desc(values)) %>%
top_n(1, values) %>%
ungroup()


plot6_age <-
ggplot(dat_age, aes(x = values, y = ind_type, fill = ind_type)) +
  geom_density_ridges(stat = "binline", alpha = 0.8, scale = 1) +
  geom_point(data = dat_age_means, aes(x = mean_val, y = ind_type), size = 2) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_fill_manual(values =
c("#1E448A",
"#3A6F9B",
"#5795AC",
"#75B6BC",
"#93CCC6",
"#B1DBCF",
"#D0EADE")) +
  labs(##title = "Is buying online different between age groups?",
       ##subtitle = "Bars made up of European countries, black dot is the median",
    x = "% with online purchases in the last 3 months",
    y = "")

plot6 <-
plot6_age + plot_gend +plot_annotation(
  title = 'Does online buying vary with age and gender?',
  subtitle = 'Bars made up of states in the EU28, black dots are the average'
)

ggsave(filename = "plot6.png",
       plot = plot6,
       height = 12,
       width = 20,
       units = "cm",
       dpi = 500)


##Who isn't buying===============================================================
unique(dat_barr$geo)
unique(dat_barr$ind_type)
unique(dat_barr$indic_is)
unique(dat_barr$time)
dat_barr %>%
  filter(time = 2019,

    str_detect(indic_is, "because"),



## graph 7: Scatter plots================================================================


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
   filter(str_detect(unit, " in the last year"),
         ind_type == "All Individuals",
         str_detect(indic_is, "encountered problems"),
         !str_detect(geo, "Euro"),
         time == 2019)

dat_purc_2019 <-
dat_purc %>%
   filter(str_detect(unit, " in the last year"),
         ind_type == "All Individuals",
         str_detect(indic_is, ": from sellers|from national"),
         !str_detect(geo, "Euro"),
         time == 2019) %>%
filter(!str_detect(indic_is, "from sellers abroad"))

dat_sells_locs <-
    dat_purc_2019 %>%
    left_join(dat_prob_2019, by = c("geo" = "geo")) %>%
    mutate(indic_is.x = str_remove(indic_is.x, "Online purchases: from")) %>%
    mutate(indic_is.x = str_to_sentence(indic_is.x)) %>%
    mutate(indic_is.x = case_when(str_detect(indic_is.x, "National") ~ "National",
                                  str_detect(indic_is.x, "eu c") ~ "EU",
                                  str_detect(indic_is.x, "non-eu") ~ "Non-EU",
                                  str_detect(indic_is.x, "unknown") ~ "Unknown"
                                  )) %>%
    mutate(indic_is.x = factor(indic_is.x, c("National", "EU", "Non-EU", "Unknown")))

dat_sells_locs %>%
filter(geo == "Austria") %>%
select(indic_is.x, indic_is.y, values.x, values.y)

dat_sells_meds <-
  dat_sells_locs %>%
  group_by(indic_is.x) %>%
  summarise(mean_x = mean(values.x, na.rm = T),
            mean_y = mean(values.y, na.rm = T)) %>%
  ungroup() %>%
  mutate(my_lab = case_when(indic_is.x == "National" ~ "Correlated with\nfewer problems",
                         indic_is.x == "EU" ~ "Some correlation\nwith problems",
                         indic_is.x == "Non-EU" ~ "Significant correlation\nwith problems",
                         indic_is.x == "Unknown" ~ "Strong correlation\nwith problems")
         )

plot7 <-
dat_sells_locs %>%
ggplot(aes(x = values.x,
           y = values.y,
           shape = indic_is.x,
           fill = indic_is.x,
           colour = indic_is.x)) +
  geom_point(alpha = 0.75) +
geom_smooth(method = "lm", se = F) +
geom_label(data = dat_sells_meds,
           aes(x = mean_x, y = mean_y, label = my_lab), size = 3, fill = "white", alpha = 0.75) +
facet_wrap(.~indic_is.x) +
theme_classic() +
theme(legend.position = "none") +
labs(x = "% of individuals who has purchased online in the last year", y = "% who have encountered problems",
     title = "The association between seller location and problems",
     subtitle = "Points are data from individual EU states",
     colour = "Seller\norigin", shape = "Seller\norigin") +
scale_colour_manual(values = c("#75B6BC", "#3A6F9B", "#1E448A", "#14326A")) +
scale_fill_manual(values = c("#75B6BC", "#3A6F9B", "#1E448A", "#14326A")) +
scale_shape_manual(values = c(21,22,23,24)) +
coord_cartesian(xlim = c(-10,110)) +
scale_x_continuous(breaks = c(0,20,40,60,80,100))

ggsave(filename = "plot7.png",
       plot = plot7,
       height = 12,
       width = 16,
       units = "cm",
       dpi = 500)


##try just a using euro region

dat_prob_2019 <-
dat_prob %>%
   filter(str_detect(unit, " in the last year"),
         ind_type == "All Individuals",
         str_detect(indic_is, "the Internet:"),
         str_detect(geo, "28 c"),
         time == 2019)

dat_purc_2019 <-
dat_purc %>%
   filter(str_detect(unit, " in the last year"),
         ind_type == "All Individuals",
         str_detect(indic_is, ": from sellers|from national"),
         str_detect(geo, "28 c"),
         time == 2019) %>%
filter(!str_detect(indic_is, "from sellers abroad"))

dat_sells_locs <-
    dat_purc_2019 %>%
    left_join(dat_prob_2019, by = c("geo" = "geo"))

dat_sells_locs %>%
ggplot(aes(x = indic_is.x, y = values.y, fill = indic_is.y)) +
geom_col()

dat_sells_locs %>%
ggplot(aes(x = values.x, y = values.y, shape = indic_is.x, colour = indic_is.x)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
##  ggpubr::stat_cor() +
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
