#load packages
#load packages
library(rio)
library(here)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)

#summarise cards
summary_cards <- rio_df_simple_panel %>%
  summarise(peruntukan = sum (peruntukan, na.rm=T),
            belanja = sum(belanja, na.rm=T),
            baki =  sum(baki, na.rm=T))
