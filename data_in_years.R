
### Title: data in, but just for all individuals, but all years
### Description: Import and local save of data

library(tidyverse)
library(here)
library(naniar)

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

df_years <-
  df %>%
  filter(ind_type == "IND_TOTAL")


colnames(df_years)[5:6] <- c("year", "val")

df_years_v <-
df_years %>%
  separate(val, c("val", "flag"))

df_years_v <-
  df_years_v %>%
  mutate(year = as.numeric(str_remove(year, "X")),
        val = as.numeric(val))


df_years_v$flag[which(df_years_v$flag == "")] <- NA



#lets just get data for 2019 for now

write_csv(df_years_v, "df_years.csv")
