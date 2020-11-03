## eurostat pacakge work

library(tidyverse)
library(eurostat)
library(knitr)
library(rvest)
library(ggridges)
library(ggrepel)
library(textshape)

##for colours
nord_frost <- c("#8FBCBB", "#88C0D0", "#81A1C1", "#5E81AC")
nord_aurora <- c("#BF616A","#D08770","#EBCB8B","#A3BE8C","#B48EAD")

nf7 <- colorRampPalette(nord_frost)(7)
nf <- colorRampPalette(nord_frost)(10)

# Get Eurostat data listing
toc <- get_eurostat_toc()

# Check the first items
library(knitr)
kable(head(toc))

id <- search_eurostat("online")$code[2]
print(id)

dat <- get_eurostat(id,
                    time_format = "num",
                    type = "label"
                    )

##search ids for some things, taken from the bottom
id_problems <- search_eurostat("buying")$code[1]
id_orders <- search_eurostat("ordered")$code[1]
id_proc <- search_eurostat("online")$code[1]
id_purc <- search_eurostat("Internet purchases")$code[1]



glimpse(dat)
unique(dat$geo)
unique(dat$indic_is)
unique(dat$unit)
unique(dat$geo)
unique(dat$ind_type)

## buying strategies
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
  scale_fill_manual(values = nord_aurora[c(1,3,5)])


##try a histogram of advertisements

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
  labs(x = "Percentage of individuals with purchases in the past year",
       y = "Age group")


## Scatter plots?================================================================

dat_purc <- get_eurostat(id_purc,
                         time_format = "num",
                         type = "label")


dat_prob <- get_eurostat(id_problems,
                       time_format = "num",
                       type = "label")

dat_purc_time <-
dat_purc %>%
  filter(unit == "Percentage of individuals",
         ind_type == "All Individuals",
         str_detect(indic_is, ": from sellers|from national"),
         str_detect(geo, "28 countries")) %>%
filter(!str_detect(indic_is, "from sellers abroad"))

unique(dat_prob$ind_type)

#a plot of purchasing locations over time
dat_purc_time %>%
  ggplot(aes(x = time, y = values, colour = indic_is, shape = indic_is)) +
  geom_path() +
  geom_point(size = 5) +
  theme_classic()

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
  labs(x = "Online purchases", y = "Problems")

# try a different one. Try a bubble one.
unique(dat_purc$ind_type)
glimpse(dat_purc_2019)


## line plot=============================================

dat_purc
unique(dat_purc$ind_type)
unique(dat_purc$indic_is)

dat_purc_lines <-
dat_purc %>%
filter(ind_type == "All Individuals",
       str_detect(indic_is, "Frequency of online"),
       str_detect(geo, "Euro area"),
       unit == "Percentage of individuals",
       !str_detect(indic_is, "6 times or more")) %>%
mutate(indic_is = str_remove(indic_is, "Frequency of online purchases in the last 3 months: ")) %>%
mutate(indic_is = str_replace(indic_is, "more", "More"))


ggplot(dat_purc_lines,
       aes(x = time,
           y = values,
           colour = indic_is,
           shape = indic_is)) +
  geom_path(alpha = 0.7) +
geom_point(size = 3) +
theme_classic()



##try a correlation matrix

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

##graph 4: Heatmap================================================================
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

dat_purc_heat <-
dat_purc %>%
  filter(time == 2019,
         !str_detect(geo, "Euro"),
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
df_purc_heat <-
dat_purc_heat %>%
select(indic_is, geo, values) %>%
pivot_wider(names_from = geo, values_from = values) %>%
as.data.frame()

rownames(df_purc_heat) <- df_purc_heat$indic_is
df_purc_heat$indic_is <- NULL

df_clust <- cluster_matrix(as.matrix(df_purc_heat), method = "average")

##make tidy for ggplot
df_c_tidy <-
df_clust %>%
as.data.frame()

df_c_tidy$indic_is <- row.names(df_c_tidy)

heat_test <-
df_c_tidy %>%
as_tibble() %>%
mutate(indic_is = fct_inorder(indic_is)) %>%
pivot_longer(cols = 1:37) %>%
mutate(name = fct_inorder(name))

`%notin%` <- Negate(`%in%`)

##more processing


heat_test %>%
ggplot(aes(y = indic_is, x = name, size = value)) +
geom_point()



##df_c_tidier <-
 ## df_c_tidy %>%
  ##filter(!str_detect(indic_is, "other travel|others|or computer software|magazines/e-learn|software, del|music, delive|papers/e-l|software, delivered"),
   ##      indic_is %notin% c(" holiday accomodation"))

##dat_purc_heat %>%
##  ggplot(aes(x = geo, y = indic_is, size = values, colour = values)) +
##  geom_point() +
##  theme_classic()

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
