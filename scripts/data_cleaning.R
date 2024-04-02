# Load Packages -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  pacman, 
  rio,
  tidyverse,
  here,
  janitor,
  dplyr,
  scales,
  googledrive,
  googlesheets4,
  ggplot2,
  plotly,
  shiny,
  shinythemes,
  flextable,
  purrr,
  DT
  ) 


# Import data -------------------------------------------------------------
# Line Project RMK 12 dataset
rio_df_sambungan <- read_sheet("https://docs.google.com/spreadsheets/d/1cVZlEypR0gK9VZUf2aQkdGMj1n73_iwbx1dLKxnZvQY/edit?usp=sharing",
                           sheet = "Sambungan")
rio_df_2021 <- read_sheet("https://docs.google.com/spreadsheets/d/1cVZlEypR0gK9VZUf2aQkdGMj1n73_iwbx1dLKxnZvQY/edit?usp=sharing",
                               sheet = "2021")
rio_df_2022 <- read_sheet("https://docs.google.com/spreadsheets/d/1cVZlEypR0gK9VZUf2aQkdGMj1n73_iwbx1dLKxnZvQY/edit?usp=sharing",
                               sheet = "2022")
rio_df_2023 <- read_sheet("https://docs.google.com/spreadsheets/d/1cVZlEypR0gK9VZUf2aQkdGMj1n73_iwbx1dLKxnZvQY/edit?usp=sharing",
                               sheet = "2023")
rio_df_2024 <- read_sheet("https://docs.google.com/spreadsheets/d/1cVZlEypR0gK9VZUf2aQkdGMj1n73_iwbx1dLKxnZvQY/edit?usp=sharing",
                               sheet = "2024")
rio_df_2025 <- read_sheet("https://docs.google.com/spreadsheets/d/1cVZlEypR0gK9VZUf2aQkdGMj1n73_iwbx1dLKxnZvQY/edit?usp=sharing",
                          sheet = "2025")

# Panel Geran 2023 dataset
rio_df_simple_panel <- read_sheet("https://docs.google.com/spreadsheets/d/19P0rOIlwc9MiEIyQqE7TxM0e0RvvfXImP_vpFOYSL78/edit?usp=sharing",
                          sheet = "simple panel")
rio_df_agihan_dan_belanja <- read_sheet("https://docs.google.com/spreadsheets/d/19P0rOIlwc9MiEIyQqE7TxM0e0RvvfXImP_vpFOYSL78/edit?usp=sharing",
                                  sheet = "Agihan & Belanja")
rio_df_gov <- read_sheet("https://docs.google.com/spreadsheets/d/19P0rOIlwc9MiEIyQqE7TxM0e0RvvfXImP_vpFOYSL78/edit?usp=sharing",
                                  sheet = "GOV")

# Data cleaning ---------------------------------------------
# Line Project RMK 12 dataset
## Sheet :  Sambungan
rio_df_sambungan <- rio_df_sambungan %>%
  row_to_names(row_number = 1) %>%  # remove row 1
  select(-c(4, 6)) %>%  # remove columns 4 and 6
  clean_names() %>%  # lower case all names
  mutate(tarikh_mula = as.Date(as.numeric(tarikh_mula), origin = "1899-12-30"),
         tarikh_tamat = as.Date(as.numeric(tarikh_tamat), origin = "1899-12-30")) %>%
  unnest(c(rmk_11, x2021, x2022, x2023, x2024, x2025, jumlah)) %>%
  mutate_at(vars(rmk_11, x2021, x2022, x2023, x2024, x2025, jumlah),
            list(~ as.numeric(.))) %>%
  select(1:16)  # Keep only the first 16 columns

## Sheet :  2021
rio_df_2021 <- rio_df_2021 %>%
  row_to_names(row_number = 1)  %>% 
  clean_names() %>%  
  mutate(tarikh_mula = as.Date(as.numeric(tarikh_mula),origin = "1899-12-30"),
         tarikh_tamat = as.Date(as.numeric(tarikh_tamat),origin = "1899-12-30")) %>%
  unnest(c(x2021_mrg, x2021_mrg_belanja, x2021_crm, x2021_crm_belanja, x2021_sukuk, 
           x2021_sukuk_belanja, x2021_kwan, x2021_mh, x2021_mh_belanja, x2021_anms, 
           x2021_anms_belanja, sumber_peruntukan2, x2022_mrg, x2023_crm, x2023, 
           x2024, x2025, jumlah)) %>%
  mutate_at(vars(x2021_mrg, x2021_mrg_belanja, x2021_crm, x2021_crm_belanja, x2021_sukuk, 
                 x2021_sukuk_belanja, x2021_kwan, x2021_mh, x2021_mh_belanja, x2021_anms, 
                 x2021_anms_belanja, sumber_peruntukan2, x2022_mrg, x2023_crm, x2023, 
                 x2024, x2025, jumlah),
            list(~ as.numeric(.))) %>%
  select(1:29)

## Sheet :  2022
rio_df_2022 <- rio_df_2022 %>%
  row_to_names(row_number = 1)  %>% 
  clean_names() %>%  
  select(-c(6)) %>%  
  mutate(tarikh_mula = as.Date(as.numeric(tarikh_mula),origin = "1899-12-30"),
         tarikh_tamat = as.Date(as.numeric(tarikh_tamat),origin = "1899-12-30")) %>%
  unnest(c(x2022, x2023, x2024, x2025, jumlah)) %>%
  mutate_at(vars(x2022, x2023, x2024, x2025, jumlah),
            list(~ as.numeric(.))) %>%
  select(1:15)  

## Sheet :  2023
rio_df_2023 <- rio_df_2023 %>%
  row_to_names(row_number = 1)  %>% 
  clean_names() %>%  
  select(-c(6)) %>%  
  mutate(tarikh_mula = as.Date(as.numeric(tarikh_mula),origin = "1899-12-30"),
         tarikh_tamat = as.Date(as.numeric(tarikh_tamat),origin = "1899-12-30")) %>%
  unnest(c(x2023, x2024, x2025, jumlah)) %>%
  mutate_at(vars(x2023, x2024, x2025, jumlah),
            list(~ as.numeric(.))) %>%
  select(1:14)

## Sheet :  2024
rio_df_2024 <- rio_df_2024 %>%
  row_to_names(row_number = 1)  %>% 
  clean_names() %>%  
  select(-c(6)) %>%  
  mutate(tarikh_mula = as.Date(as.numeric(tarikh_mula),origin = "1899-12-30"),
         tarikh_tamat = as.Date(as.numeric(tarikh_tamat),origin = "1899-12-30")) %>%
  unnest(c(x2024, x2025, x2026, x2027, jumlah)) %>%
  mutate_at(vars(x2024, x2025, x2026, x2027, jumlah),
            list(~ as.numeric(.))) %>%
  select(1:15)

## Sheet :  2025
rio_df_2025 <- rio_df_2025 %>%
  row_to_names(row_number = 1)  %>% 
  clean_names() %>%  
  select(-c(6)) %>%  
  mutate(tarikh_mula = as.Date(as.numeric(tarikh_mula),origin = "1899-12-30"),
         tarikh_tamat = as.Date(as.numeric(tarikh_tamat),origin = "1899-12-30")) %>%
  unnest(c(x2025,jumlah)) %>%
  mutate_at(vars(x2025, jumlah),
            list(~ as.numeric(.))) %>%
  select(1:15)

# Panel Geran 2023
## Sheet :  Simple Panel
rio_df_simple_panel <- rio_df_simple_panel %>%
  clean_names()  %>%
  unnest(c(peruntukan, pulang_balik, belanja, baki)) %>%
  mutate(tahun_mula = as.Date(as.numeric(tahun_mula), origin = "1899-12-30"),
         tahun_tamat = as.Date(as.numeric(tahun_tamat), origin = "1899-12-30"),
         year = as.Date(year, format = "%Y")) %>%
  mutate_at(vars(peruntukan, pulang_balik, belanja, baki),
            list(~ as.numeric(.))) %>%
  select(1:20) %>%
  fill(nmrr, .direction = "down")


## Sheet : Agihan & Belanja
rio_df_agihan_dan_belanja <- rio_df_agihan_dan_belanja %>%
  row_to_names(row_number = 2) %>%
  clean_names()  
rio_df_agihan_dan_belanja <- rio_df_agihan_dan_belanja %>%
  mutate_at(c("peruntukan_2023", "auto_sum", "pulang_balik", "peruntukan1", "belanja1",
                 "peruntukan2", "belanja2", "peruntukan3", "belanja3","peruntukan4", "belanja4",
                 "peruntukan5", "belanja5", "peruntukan6", "belanja6", "peruntukan7", "belanja7",
                 "peruntukan8", "belanja8", "peruntukan9", "belanja9", "peruntukan10", "belanja10"),
            list(~ as.numeric(as.character(.)))) %>%
  fill(nmrr, .direction = "down") %>%
  select(1:50) 

## Sheet:  GOV
rio_df_gov_2023 <- rio_df_gov %>% 
  slice(1:10) %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(
    peruntukan = as.numeric(str_replace_all(peruntukan, "[^0-9.]", "")),
    agihan = as.numeric(str_replace_all(agihan, "[^0-9.]", "")),
    baki = as.numeric(str_replace_all(baki, "[^0-9.]", "")),
    belanja = as.numeric(str_replace_all(belanja, "[^0-9.]", ""))
  )


rio_df_gov_2024 <- rio_df_gov %>% 
  slice(14:23) %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(peruntukan = as.numeric(str_replace_all(peruntukan, "[^0-9.]", "")))

