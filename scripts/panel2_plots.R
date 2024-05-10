#load packages
library(rio)
library(here)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(janitor)
library(scales)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(flextable)
library(DT)
library(googledrive)
library(googlesheets4)
library(jsonlite)


# Data visualizations -----------------------------------------------------
## 3 level (GOV, NIH_belanja, NIH_baki)
rio_df_simple_panel_cluster <- rio_df_simple_panel %>%
  group_by(cluster) %>%
  summarize(total_peruntukan = sum(peruntukan, na.rm = TRUE),
            total_belanja = sum(belanja, na.rm = TRUE)) %>%
  mutate(total_peruntukan_million = total_peruntukan / 1e6,
         total_belanja_million = total_belanja / 1e6) 

merged_df <- merge(rio_df_gov_2023, rio_df_simple_panel_cluster, by = "cluster", all = FALSE)

rio_df_merged_plot <- merged_df %>%
  group_by(cluster) %>%
  mutate(peruntukan_GOV = peruntukan / 1e6)

plot_merged_absolute <- ggplot(rio_df_merged_plot %>%
                                 mutate(Peruntukan_dari_Kerajaan = peruntukan_GOV - total_peruntukan_million,
                                        Peruntukan_dari_NIH = total_peruntukan_million - total_belanja_million,
                                        Baki_dari_NIH = total_belanja_million) %>%
                                 select(cluster, Peruntukan_dari_Kerajaan, Peruntukan_dari_NIH, Baki_dari_NIH) %>%
                                 pivot_longer(-cluster, 
                                              values_to = "million",
                                              names_to = "level"), 
                               aes(fill=level, y=cluster, x=million)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = scales::comma(million, 
                                      accuracy = 0.1,
                                      decimal.mark = "."), 
                y = cluster),
            position = position_stack(vjust = 0.5), check_overlap = T,
            color = "black",
            size = 3) +
  theme_minimal() +
  labs(x = "Million (MYR)", y = "Research Grant Cluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("Distribution of Allocations by Government and NIH") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Peruntukan_dari_Kerajaan" = "#5fa8d3", "Peruntukan_dari_NIH" = "#3c8a99", "Baki_dari_NIH" = "#64b9b9"),
                    labels = c(Peruntukan_dari_Kerajaan = "Allocation (GOV)", Peruntukan_dari_NIH = "Allocation (NIH)", Baki_dari_NIH = "Balance from NIH"))


# TRIAL -------------------------------------------------------------------

#new

rio_df_gov_2023_new <- rio_df_gov_2023 %>%
  select(cluster, agihan, peruntukan, baki)

plot_merged_absolute2 <- ggplot(rio_df_gov_2023_new %>%
                                 mutate(Peruntukan_dari_Kerajaan = peruntukan,
                                        Peruntukan_diguna = agihan,
                                        Baki_dari_NIH = baki) %>%
                                 select(cluster, Peruntukan_dari_Kerajaan, Peruntukan_diguna, Baki_dari_NIH) %>%
                                 pivot_longer(-cluster, 
                                              values_to = "million",
                                              names_to = "level"), 
                               aes(fill=level, y=cluster, x=million)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = scales::comma(million, 
                                      accuracy = 0.1,
                                      decimal.mark = "."), 
                y = cluster),
            position = position_stack(vjust = 0.5), check_overlap = TRUE,
            color = "black",
            size = 3) +
  theme_minimal() +
  labs(x = "Million (MYR)", y = "Research Grant Cluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("Distribution of Allocations by Government and NIH") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Peruntukan_dari_Kerajaan" = "#5fa8d3", "Peruntukan_diguna" = "#3c8a99", "Baki_dari_NIH" = "#64b9b9"),
                    labels = c("Peruntukan_dari_Kerajaan" = "Allocation (GOV)", "Peruntukan_diguna" = "Allocation (NIH)", "Baki_dari_NIH" = "Balance from NIH"))

# TRIAL -------------------------------------------------------------------


##Percentage
plot_merged_percent <- ggplot(rio_df_merged_plot %>%
                                mutate(Peruntukan_dari_Kerajaan = (peruntukan_GOV - total_peruntukan_million),
                                       Peruntukan_dari_NIH = (total_peruntukan_million - total_belanja_million),
                                       Peratus_peruntukan = (Peruntukan_dari_Kerajaan / (Peruntukan_dari_NIH + Peruntukan_dari_Kerajaan + total_belanja_million))*100,
                                       Peratus_baki = (Peruntukan_dari_NIH / (Peruntukan_dari_NIH + Peruntukan_dari_Kerajaan + total_belanja_million))*100,
                                       Peratus_belanja = (total_belanja_million / (Peruntukan_dari_NIH + Peruntukan_dari_Kerajaan + total_belanja_million))*100) %>%
                                select(cluster, Peratus_peruntukan,Peratus_baki, Peratus_belanja) %>%
                                pivot_longer(-cluster, 
                                             values_to = "percentage",
                                             names_to = "level"), 
                              aes(fill=level, y=cluster, x=percentage)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = scales::comma(percentage, 
                                      accuracy = 0.1,
                                      decimal.mark = "."), 
                y = cluster),
            position = position_stack(vjust = 0.5), check_overlap = T,
            color = "black",
            size = 3) +
  theme_minimal() +
  labs(x = "Peratus (%)", y = "Research Grant Cluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("Distribution of Allocations by Government and NIH (%)") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Peratus_peruntukan" = "#5fa8d3", "Peratus_baki" = "#3c8a99", "Peratus_belanja" = "#64b9b9"),
                    labels = c(Peratus_peruntukan = "Allocation (GOV), %", Peratus_baki = "Allocation (NIH), %", Peratus_belanja = "Balance from NIH (%)"))

## 2 levels - Peruntukan dan belanja by PTJ
rio_df_simple_panel_ptj <- rio_df_simple_panel %>%
  group_by(ptj) %>%
  summarize(total_peruntukan = sum(peruntukan, na.rm = TRUE),
            total_belanja = sum(belanja, na.rm = TRUE)) %>%
  mutate(total_peruntukan_million = total_peruntukan / 1e6,
         total_belanja_million = total_belanja / 1e6) 

plot_ptj_absolute <- ggplot(rio_df_simple_panel_ptj %>%
                              mutate(Belanja_dari_NIH = total_belanja_million,
                                     Baki_dari_NIH = total_peruntukan_million - total_belanja_million) %>%
                              select(ptj, Belanja_dari_NIH, Baki_dari_NIH) %>%
                              pivot_longer(-ptj, 
                                           values_to = "million",
                                           names_to = "level"), 
                            aes(fill=level, y=ptj, x=million)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = scales::comma(million, 
                                      accuracy = 0.1,
                                      decimal.mark = "."), 
                y = ptj),
            position = position_stack(vjust = 0.5), check_overlap = T,
            color = "black",
            size = 3) +
  theme_minimal() +
  labs(x = "Million (MYR)", y = "PTJ", fill = "") +
  theme(legend.position = "top") +
  ggtitle("Expenses and Balances from NIH (MYR)") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_dari_NIH" = "#64b9b9","Baki_dari_NIH" = "#3c8a99"),
                    labels = c(Belanja_dari_NIH = "Amount Spent", Baki_dari_NIH = "Amount Left"))


#Percentage
rio_df_simple_panel_percentage_ptj <- rio_df_simple_panel %>%
  group_by(ptj) %>%
  summarize(total_peruntukan = sum(peruntukan, na.rm = TRUE),
            total_belanja = sum(belanja, na.rm = TRUE)) 


plot_ptj_percent <- ggplot(rio_df_simple_panel_percentage_ptj %>%
                             mutate(Belanja_dari_NIH = (total_belanja/total_peruntukan)*100,
                                    Baki_dari_NIH=100-Belanja_dari_NIH) %>%
                             select(ptj, Belanja_dari_NIH, Baki_dari_NIH) %>%
                             pivot_longer(-ptj, 
                                          values_to = "percentage",
                                          names_to = "level"), 
                           aes(fill=level, y=ptj, x=percentage)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  theme_minimal() +
  labs(x = "Peratus (%)", y = "PTJ", fill = "") +
  theme(legend.position = "top") +
  ggtitle("Expenses and Balances from NIH (%)") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_dari_NIH" = "#64b9b9","Baki_dari_NIH" = "#3c8a99"),
                    labels = c(Belanja_dari_NIH = "Amount Spent", Baki_dari_NIH = "Amount Left"))