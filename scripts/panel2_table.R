#load packages
library(rio)
library(here)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

overspent_df <- rio_df_simple_panel %>%
  select(tajuk, ptj, cluster, peruntukan, belanja, baki) %>%
  rename(Project = tajuk,
         Institute = ptj,
         Cluster = cluster,
         Allocation = peruntukan,
         Spent = belanja,
         Balance = baki) %>%
  filter(Spent > Allocation) %>%
  mutate(
    Allocation = paste("MYR", Allocation),
    Spent = paste("MYR", Spent),
    Balance = paste("MYR", round(Balance, 2))  # Round Balance column to 2 decimal places
  )
  