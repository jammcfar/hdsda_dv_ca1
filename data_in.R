### Title: data in
### Description: Import and local save of data

library(tidyverse)
library(here)
library(naniar)

## compile dictionaries
dic_files <- list.files(paste(here(), "/data/dic", sep = "/"),
                        full.names = T)

dic_list <- list()
for(i in 1:length(dic_files)){

  dic_list[[i]] <- read_tsv(dic_files[i], col_names = F)

}

dic_df <- do.call(rbind, dic_list)
dic_df_d <- distinct(dic_df)

colnames(dic_df_d) <- c("code", "details")

i_find <- grep("internet", dic_df_d$details, ignore.case = T)
dic_df_d[i_find, ] #yeah its good

write_csv(dic_df_d, "dict.csv")

##read in data filesa

## Get names in file
files_2_get <- list.files(here("data/internet"))

d_list = list()

for(i in 1:length(files_2_get)){

  foo_lines <- readLines(paste0(here(),
                              "/data/internet/",
                              files_2_get[i])
                         )

  foo_lines_subbed <- gsub(",", "\t", foo_lines)

#  head(read.table(textConnection(foo_lines_subbed)))

  foo_tab <- read.table(textConnection(foo_lines_subbed),
                        header = T,
                        fill = T,
                        sep = "\t",
                        stringsAsFactors = F)

  #could do something like find the biggest year string, but lets see how this goes
  foo_long <-
    foo_tab %>%
    pivot_longer(cols = 5:ncol(foo_tab))

  d_list[[files_2_get[i]]] <- foo_long

 #foo <- read.table( text = gsub(",",
 #                              "\t",
 #                                        header = T,
 #                                        fill = T,
 #                                        sep = "\t",
 #                                        stringsAsFactors = F)
  gc()

}

glimpse(d_list)

df <- do.call(rbind, d_list)

##clean data
gc()

colnames(df)[5:6] <- c("year", "val")

rm(df)

gc()


df_sep <-
df %>%
  separate(val, c("val", "flag"))

rm(df)

gc()

df_sep_v <-
  df_sep %>%
  mutate(year = as.numeric(str_remove(year, "X")),
        val = as.numeric(val))


df_sep_v$flag[which(df_sep_v$flag == "")] <- NA

glimpse(df_sep_v)

rm(df_sep)
gc()

#lets just get data for 2019 for now
df_2019 <-
df_sep_v %>%
  filter(year == 2019)

glimpse(df_2019)

write_csv(df_2019, "df_individuals.csv")
