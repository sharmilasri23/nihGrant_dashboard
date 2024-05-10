#load packages
library(rio)
library(here)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

#create subfiltered dataset

#2021
rio_df_2021_new <- rio_df_sambungan %>%
  select(tajuk_projek, kluster, jabatan, sumber_peruntukan, x2021) %>%
  rename(Project = tajuk_projek,
         Cluster = kluster, 
         Institute = jabatan,
         Allocation = x2021,
         Source = sumber_peruntukan)
rio_df_2021_new$Allocation <- paste("MYR", rio_df_2021_new$Allocation)

#2022
rio_df_2022_new <- rio_df_2022%>%
  select(tajuk_projek, kluster, jabatan, sumber_peruntukan, x2022) %>%
  rename(Project = tajuk_projek,
         Cluster = kluster, 
         Institute = jabatan,
         Allocation = x2022,
         Source = sumber_peruntukan)
rio_df_2022_new$Allocation <- paste("MYR", rio_df_2022_new$Allocation)

#2023
rio_df_2023_new <- rio_df_2023%>%
  select(tajuk_projek, kluster, jabatan, sumber_peruntukan, x2023) %>%
  rename(Project = tajuk_projek,
         Cluster = kluster, 
         Institute = jabatan,
         Allocation = x2023,
         Source = sumber_peruntukan)
rio_df_2023_new$Allocation <- paste("MYR", rio_df_2023_new$Allocation)

#2024
rio_df_2024_new <- rio_df_2024%>%
  select(tajuk_projek, kluster, jabatan, peruntukan, x2024) %>%
  rename(Project = tajuk_projek,
         Cluster = kluster, 
         Institute = jabatan,
         Allocation = x2024,
         Source = peruntukan)
rio_df_2024_new$Allocation <- paste("MYR", rio_df_2024_new$Allocation)

#2025
rio_df_2025_new <- rio_df_2025%>%
  select(tajuk_projek, kluster, jabatan, sumber_peruntukan, x2025) %>%
  rename(Project = tajuk_projek,
         Cluster = kluster, 
         Institute = jabatan,
         Allocation = x2025,
         Source = sumber_peruntukan)
rio_df_2025_new$Allocation <- paste("MYR", rio_df_2025_new$Allocation)