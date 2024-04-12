#load packages
library(rio)
library(here)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(scales)
library(plotly)

## Trend line (2021, 2022, 2023, 2024)
#by Cluster- allocation
rio_df_2021_new <- rio_df_sambungan %>%
  select(kluster, x2021) %>%
  group_by(kluster) %>%
  summarise(n = sum(x2021)) %>%
  mutate(year = 2021,
         level = "kluster") %>%
  rename(sub_level = kluster) %>%
  na.omit()

rio_df_2022_new <- rio_df_2022 %>%
  select(kluster, x2022) %>%
  group_by(kluster) %>%
  summarise(n = sum(x2022)) %>%
  mutate(year = 2022,
         level = "kluster") %>%
  rename(sub_level = kluster) %>%
  na.omit()

rio_df_2023_new <- rio_df_2023 %>%
  select(kluster, x2023) %>%
  group_by(kluster) %>%
  summarise(n = sum(x2023)) %>%
  mutate(year = 2023,
         level = "kluster") %>%
  rename(sub_level = kluster) %>%
  na.omit()

rio_df_2024_new <- rio_df_2024 %>%
  select(kluster, x2024) %>%
  group_by(kluster) %>%
  summarise(n = sum(x2024)) %>%
  mutate(year = 2024,
         level = "kluster") %>%
  rename(sub_level = kluster) %>%
  na.omit()

rio_df_2025_new <- rio_df_2025 %>%
  select(kluster, x2025) %>%
  group_by(kluster) %>%
  summarise(n = sum(x2025)) %>%
  mutate(year = 2025,
         level = "kluster") %>%
  rename(sub_level = kluster) %>%
  na.omit()

# by PTJ- allocation
rio_df_2021_new2 <- rio_df_sambungan %>%
  select(jabatan, x2021) %>%
  group_by(jabatan) %>%
  summarise(n = sum(x2021)) %>%
  mutate(year = 2021,
         level = "jabatan") %>%
  rename(sub_level = jabatan) %>%
  na.omit()

rio_df_2022_new2 <- rio_df_2022 %>%
  select(jabatan, x2022) %>%
  group_by(jabatan) %>%
  summarise(n = sum(x2022)) %>%
  mutate(year = 2022,
         level = "jabatan") %>%
  rename(sub_level = jabatan) %>%
  na.omit()

rio_df_2023_new2 <- rio_df_2023 %>%
  select(jabatan, x2023) %>%
  group_by(jabatan) %>%
  summarise(n = sum(x2023)) %>%
  mutate(year = 2023,
         level = "jabatan") %>%
  rename(sub_level = jabatan) %>%
  na.omit()

rio_df_2024_new2 <- rio_df_2024 %>%
  select(jabatan, x2024) %>%
  group_by(jabatan) %>%
  summarise(n = sum(x2024)) %>%
  mutate(year = 2024,
         level = "jabatan") %>%
  rename(sub_level = jabatan) %>%
  na.omit()

rio_df_2025_new2 <- rio_df_2025 %>%
  select(jabatan, x2025) %>%
  group_by(jabatan) %>%
  summarise(n = sum(x2025)) %>%
  mutate(year = 2025,
         level = "jabatan") %>%
  rename(sub_level = jabatan) %>%
  na.omit()

#Overall allocation
rio_df_2021_new3 <- rio_df_sambungan %>%
  select(x2021) %>%
  summarise(n = sum(x2021)) %>%
  mutate(year = 2021,
         level = "overall",
         sub_level = 'Overall') %>%
  na.omit()

rio_df_2022_new3 <- rio_df_2022 %>%
  select(jabatan, x2022) %>%
  summarise(n = sum(x2022)) %>%
  mutate(year = 2022,
         level = "overall",
         sub_level = 'Overall') %>%
  na.omit()

rio_df_2023_new3 <- rio_df_2023 %>%
  select(x2023) %>%
  summarise(n = sum(x2023)) %>%
  mutate(year = 2023,
         level = "overall",
         sub_level = 'Overall') %>%
  na.omit()

rio_df_2024_new3 <- rio_df_2024 %>%
  select(x2024) %>%
  summarise(n = sum(x2024)) %>%
  mutate(year = 2024,
         level = "overall",
         sub_level = 'Overall') %>%
  na.omit()

rio_df_2025_new3 <- rio_df_2025 %>%
  select(x2025) %>%
  summarise(n = sum(x2025)) %>%
  mutate(year = 2025,
         level = "overall",
         sub_level = 'Overall') %>%
  na.omit()

# Append the datasets together
trend_df <- bind_rows(rio_df_2021_new,
                      rio_df_2022_new,
                      rio_df_2023_new,
                      rio_df_2024_new,
                      rio_df_2025_new,
                      rio_df_2021_new2,
                      rio_df_2022_new2,
                      rio_df_2023_new2,
                      rio_df_2024_new2,
                      rio_df_2025_new2,
                      rio_df_2021_new3,
                      rio_df_2022_new3,
                      rio_df_2023_new3,
                      rio_df_2024_new3,
                      rio_df_2025_new3)

# Filter data for 'overall' level and plot
overall_df <- trend_df %>%
  filter(level == "overall") %>%
  mutate(year = as.factor(year))

# Create a ggplot time series chart
overall_df_plot <- ggplot(data = overall_df, 
                          aes(x = year, y = n/1e6, group = sub_level, color = sub_level)) +
  geom_line(size = 1.2) +  # Draw lines
  scale_y_continuous(labels = scales::label_number(suffix = "M")) +
  scale_color_viridis_d() +  # Use viridis color scale
  labs(title = "Trends of Grant allocation in NIH",
       x = "Year",
       y = "Allocation (MYR)",
       color = NULL) +
  theme_minimal() +  # Use a minimal theme
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        legend.position = "bottom")  # Place legend at the bottom

# Filter data for 'kluster' level and plot
kluster_df <- trend_df %>%
  filter(level == "kluster") %>%
  mutate(year = as.factor(year))

# Create a ggplot time series chart
kluster_df_plot <- ggplot(data = kluster_df, 
                          aes(x = year, y = n/1e6, group = sub_level, color = sub_level)) +
  geom_line(size = 1.2) +  # Draw lines
  scale_y_continuous(labels = scales::label_number(suffix = "M")) +
  scale_color_viridis_d() +  # Use viridis color scale
  labs(title = "Trends of Grant allocation in NIH",
       x = "Year",
       y = "Allocation (MYR)",
       color = NULL) +
  theme_minimal() +  # Use a minimal theme
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        legend.position = "bottom")  # Place legend at the bottom


# Filter data for 'jabatan' level and plot
jabatan_df <- trend_df %>%
  filter(level == "jabatan") %>%
  mutate(year = as.factor(year))

# Create a ggplot time series chart
jabatan_df_plot <- ggplot(data = jabatan_df, 
                          aes(x = year, y = n/1e6, group = sub_level, color = sub_level)) +
  geom_line(size = 1.2) +  # Draw lines
  scale_y_continuous(labels = scales::label_number(suffix = "M")) +
  scale_color_viridis_d() +  # Use viridis color scale
  labs(title = "Trends of Grant allocation in NIH",
       x = "Year",
       y = "Allocation (MYR)",
       color = NULL) +
  theme_minimal() +  # Use a minimal theme
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        legend.position = "bottom")  # Place legend at the bottom
