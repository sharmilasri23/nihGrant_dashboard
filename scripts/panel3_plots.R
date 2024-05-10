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

# plot 1 ------------------------------------------------------------------

rio_df_agihan_dan_belanja_new_cluster1 <- rio_df_agihan_dan_belanja %>%
  group_by(kluster) %>%
  summarize(
    total_belanja1 = sum(belanja1, na.rm = TRUE),
    total_peruntukan1 = sum(peruntukan1, na.rm = TRUE)) %>%
  mutate(total_belanja1_million = total_belanja1 / 1e6,
         total_peruntukan1_million = total_peruntukan1 /1e6)
agihan1 <- ggplot(rio_df_agihan_dan_belanja_new_cluster1 %>%
                    mutate(Belanja_oleh_NIH = (total_belanja1_million/total_peruntukan1_million)*100,
                           Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                    select(kluster,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
                    pivot_longer(-kluster, 
                                 values_to = "percentage",
                                 names_to = "level"), 
                  aes(fill=level, y=kluster, x=percentage)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  theme_minimal() +
  labs(x = "Peratus (%)", y = "Kluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 1", 
                               Baki_oleh_NIH = "Balance from Distribution 1"))



# plot 2 ------------------------------------------------------------------


rio_df_agihan_dan_belanja_new_cluster2 <- rio_df_agihan_dan_belanja %>%
  group_by(kluster) %>%
  summarize(
    total_belanja2 = sum(belanja2, na.rm = TRUE),
    total_peruntukan2 = sum(peruntukan2, na.rm = TRUE)) %>%
  mutate(total_belanja2_million = total_belanja2 / 1e6,
         total_peruntukan2_million = total_peruntukan2 /1e6)
agihan2 <- ggplot(rio_df_agihan_dan_belanja_new_cluster2 %>%
                    mutate(Belanja_oleh_NIH = (total_belanja2_million/total_peruntukan2_million)*100,
                           Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                    select(kluster,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
                    pivot_longer(-kluster, 
                                 values_to = "percentage",
                                 names_to = "level"), 
                  aes(fill=level, y=kluster, x=percentage)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  theme_minimal() +
  labs(x = "Peratus (%)", y = "Kluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 2", 
                               Baki_oleh_NIH = "Balance from Distribution 2"))



# plot 3 ------------------------------------------------------------------

rio_df_agihan_dan_belanja_new_cluster3 <- rio_df_agihan_dan_belanja %>%
  group_by(kluster) %>%
  summarize(
    total_belanja3 = sum(belanja3, na.rm = TRUE),
    total_peruntukan3 = sum(peruntukan3, na.rm = TRUE)) %>%
  mutate(total_belanja3_million = total_belanja3 / 1e6,
         total_peruntukan3_million = total_peruntukan3 /1e6)
agihan3 <- ggplot(rio_df_agihan_dan_belanja_new_cluster3 %>%
                    mutate(Belanja_oleh_NIH = (total_belanja3_million/total_peruntukan3_million)*100,
                           Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                    select(kluster,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
                    pivot_longer(-kluster, 
                                 values_to = "percentage",
                                 names_to = "level"), 
                  aes(fill=level, y=kluster, x=percentage)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  theme_minimal() +
  labs(x = "Peratus (%)", y = "Kluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 3", 
                               Baki_oleh_NIH = "Balance from Distribution 3"))

# plot 4 ------------------------------------------------------------------

rio_df_agihan_dan_belanja_new_cluster4 <- rio_df_agihan_dan_belanja %>%
  group_by(kluster) %>%
  summarize(
    total_belanja4 = sum(belanja4, na.rm = TRUE),
    total_peruntukan4 = sum(peruntukan4, na.rm = TRUE)) %>%
  mutate(total_belanja4_million = total_belanja4 / 1e6,
         total_peruntukan4_million = total_peruntukan4 /1e6)
agihan4 <- ggplot(rio_df_agihan_dan_belanja_new_cluster4 %>%
                    mutate(Belanja_oleh_NIH = (total_belanja4_million/total_peruntukan4_million)*100,
                           Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                    select(kluster,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
                    pivot_longer(-kluster, 
                                 values_to = "percentage",
                                 names_to = "level"), 
                  aes(fill=level, y=kluster, x=percentage)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  theme_minimal() +
  labs(x = "Peratus (%)", y = "Kluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 4", 
                               Baki_oleh_NIH = "Balance from Distribution 4"))


# plot 5 ------------------------------------------------------------------

rio_df_agihan_dan_belanja_new_cluster5 <- rio_df_agihan_dan_belanja %>%
  group_by(kluster) %>%
  summarize(
    total_belanja5 = sum(belanja5, na.rm = TRUE),
    total_peruntukan5 = sum(peruntukan5, na.rm = TRUE)) %>%
  mutate(total_belanja5_million = total_belanja5 / 1e6,
         total_peruntukan5_million = total_peruntukan5 /1e6)
agihan5 <- ggplot(rio_df_agihan_dan_belanja_new_cluster5 %>%
                    mutate(Belanja_oleh_NIH = (total_belanja5_million/total_peruntukan5_million)*100,
                           Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                    select(kluster,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
                    pivot_longer(-kluster, 
                                 values_to = "percentage",
                                 names_to = "level"), 
                  aes(fill=level, y=kluster, x=percentage)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  theme_minimal() +
  labs(x = "Peratus (%)", y = "Kluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 5", 
                               Baki_oleh_NIH = "Balance from Distribution 5"))


# plot 6 ------------------------------------------------------------------

rio_df_agihan_dan_belanja_new_cluster6 <- rio_df_agihan_dan_belanja %>%
  group_by(kluster) %>%
  summarize(
    total_belanja6 = sum(belanja6, na.rm = TRUE),
    total_peruntukan6 = sum(peruntukan6, na.rm = TRUE)) %>%
  mutate(total_belanja6_million = total_belanja6 / 1e6,
         total_peruntukan6_million = total_peruntukan6 /1e6)
agihan6 <- ggplot(rio_df_agihan_dan_belanja_new_cluster6 %>%
                    mutate(Belanja_oleh_NIH = (total_belanja6_million/total_peruntukan6_million)*100,
                           Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                    select(kluster,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
                    pivot_longer(-kluster, 
                                 values_to = "percentage",
                                 names_to = "level"), 
                  aes(fill=level, y=kluster, x=percentage)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  theme_minimal() +
  labs(x = "Peratus (%)", y = "Kluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 6", 
                               Baki_oleh_NIH = "Balance from Distribution 6"))


# plot 7 ------------------------------------------------------------------

rio_df_agihan_dan_belanja_new_cluster7 <- rio_df_agihan_dan_belanja %>%
  group_by(kluster) %>%
  summarize(
    total_belanja7 = sum(belanja7, na.rm = TRUE),
    total_peruntukan7 = sum(peruntukan7, na.rm = TRUE)) %>%
  mutate(total_belanja7_million = total_belanja7 / 1e6,
         total_peruntukan7_million = total_peruntukan7 /1e6)
agihan7 <- ggplot(rio_df_agihan_dan_belanja_new_cluster7 %>%
                    mutate(Belanja_oleh_NIH = (total_belanja7_million/total_peruntukan7_million)*100,
                           Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                    select(kluster,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
                    pivot_longer(-kluster, 
                                 values_to = "percentage",
                                 names_to = "level"), 
                  aes(fill=level, y=kluster, x=percentage)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  theme_minimal() +
  labs(x = "Peratus (%)", y = "Kluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 7", 
                               Baki_oleh_NIH = "Balance from Distribution 7"))


# plot 8 ------------------------------------------------------------------

rio_df_agihan_dan_belanja_new_cluster8 <- rio_df_agihan_dan_belanja %>%
  group_by(kluster) %>%
  summarize(
    total_belanja8 = sum(belanja8, na.rm = TRUE),
    total_peruntukan8 = sum(peruntukan8, na.rm = TRUE)) %>%
  mutate(total_belanja8_million = total_belanja8 / 1e6,
         total_peruntukan8_million = total_peruntukan8 /1e6)
agihan8 <- ggplot(rio_df_agihan_dan_belanja_new_cluster8 %>%
                    mutate(Belanja_oleh_NIH = (total_belanja8_million/total_peruntukan8_million)*100,
                           Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                    select(kluster,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
                    pivot_longer(-kluster, 
                                 values_to = "percentage",
                                 names_to = "level"), 
                  aes(fill=level, y=kluster, x=percentage)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  theme_minimal() +
  labs(x = "Peratus (%)", y = "Kluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 8", 
                               Baki_oleh_NIH = "Balance from Distribution 8"))


# plot 9 ------------------------------------------------------------------

rio_df_agihan_dan_belanja_new_cluster9 <- rio_df_agihan_dan_belanja %>%
  group_by(kluster) %>%
  summarize(
    total_belanja9 = sum(belanja9, na.rm = TRUE),
    total_peruntukan9 = sum(peruntukan9, na.rm = TRUE)) %>%
  mutate(total_belanja9_million = total_belanja9 / 1e6,
         total_peruntukan9_million = total_peruntukan9 /1e6)
agihan9 <- ggplot(rio_df_agihan_dan_belanja_new_cluster9 %>%
                    mutate(Belanja_oleh_NIH = (total_belanja9_million/total_peruntukan9_million)*100,
                           Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                    select(kluster,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
                    pivot_longer(-kluster, 
                                 values_to = "percentage",
                                 names_to = "level"), 
                  aes(fill=level, y=kluster, x=percentage)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  theme_minimal() +
  labs(x = "Peratus (%)", y = "Kluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 9", 
                               Baki_oleh_NIH = "Balance from Distribution 9"))


# plot 10 -----------------------------------------------------------------

rio_df_agihan_dan_belanja_new_cluster10 <- rio_df_agihan_dan_belanja %>%
  group_by(kluster) %>%
  summarize(
    total_belanja10 = sum(belanja10, na.rm = TRUE),
    total_peruntukan10 = sum(peruntukan10, na.rm = TRUE)) %>%
  mutate(total_belanja10_million = total_belanja10 / 1e6,
         total_peruntukan10_million = total_peruntukan10 /1e6)
agihan10 <- ggplot(rio_df_agihan_dan_belanja_new_cluster10 %>%
                     mutate(Belanja_oleh_NIH = (total_belanja10_million/total_peruntukan10_million)*100,
                            Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                     select(kluster,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
                     pivot_longer(-kluster, 
                                  values_to = "percentage",
                                  names_to = "level"), 
                   aes(fill=level, y=kluster, x=percentage)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "black") +
  theme_minimal() +
  labs(x = "Peratus (%)", y = "Kluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 10", 
                               Baki_oleh_NIH = "Balance from Distribution 10"))





## PTJ TABLE PLOTS ---------------------------------------------------------
# Plot 1

rio_df_agihan_dan_belanja_ptj1 <- rio_df_agihan_dan_belanja %>%
  group_by(ptj) %>%
  summarize(
    total_belanja1 = sum(belanja1, na.rm = TRUE),
    total_peruntukan1 = sum(peruntukan1, na.rm = TRUE)) %>%
  mutate(total_belanja1_million = total_belanja1 / 1e6,
         total_peruntukan1_million = total_peruntukan1 /1e6)
ptj1 <- ggplot(rio_df_agihan_dan_belanja_ptj1 %>%
                 mutate(Belanja_oleh_NIH = (total_belanja1_million/total_peruntukan1_million)*100,
                        Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                 select(ptj,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
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
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 1", 
                               Baki_oleh_NIH = "Balance from Distribution 1"))

# Plot 2

rio_df_agihan_dan_belanja_ptj2 <- rio_df_agihan_dan_belanja %>%
  group_by(ptj) %>%
  summarize(
    total_belanja2 = sum(belanja2, na.rm = TRUE),
    total_peruntukan2 = sum(peruntukan2, na.rm = TRUE)) %>%
  mutate(total_belanja2_million = total_belanja2 / 1e6,
         total_peruntukan2_million = total_peruntukan2 /1e6)
ptj2 <- ggplot(rio_df_agihan_dan_belanja_ptj2 %>%
                 mutate(Belanja_oleh_NIH = (total_belanja2_million/total_peruntukan2_million)*100,
                        Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                 select(ptj,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
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
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 2", 
                               Baki_oleh_NIH = "Balance from Distribution 2"))


# Plot 3

rio_df_agihan_dan_belanja_ptj3 <- rio_df_agihan_dan_belanja %>%
  group_by(ptj) %>%
  summarize(
    total_belanja3 = sum(belanja3, na.rm = TRUE),
    total_peruntukan3 = sum(peruntukan3, na.rm = TRUE)) %>%
  mutate(total_belanja3_million = total_belanja3 / 1e6,
         total_peruntukan3_million = total_peruntukan3 /1e6)
ptj3 <- ggplot(rio_df_agihan_dan_belanja_ptj3 %>%
                 mutate(Belanja_oleh_NIH = (total_belanja3_million/total_peruntukan3_million)*100,
                        Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                 select(ptj,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
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
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 3", 
                               Baki_oleh_NIH = "Balance from Distribution 3"))

# Plot 4

rio_df_agihan_dan_belanja_ptj4 <- rio_df_agihan_dan_belanja %>%
  group_by(ptj) %>%
  summarize(
    total_belanja4 = sum(belanja4, na.rm = TRUE),
    total_peruntukan4 = sum(peruntukan4, na.rm = TRUE)) %>%
  mutate(total_belanja4_million = total_belanja4 / 1e6,
         total_peruntukan4_million = total_peruntukan4 /1e6)
ptj4 <- ggplot(rio_df_agihan_dan_belanja_ptj4 %>%
                 mutate(Belanja_oleh_NIH = (total_belanja4_million/total_peruntukan4_million)*100,
                        Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                 select(ptj,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
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
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 4", 
                               Baki_oleh_NIH = "Balance from Distribution 4"))

# Plot 5

rio_df_agihan_dan_belanja_ptj5 <- rio_df_agihan_dan_belanja %>%
  group_by(ptj) %>%
  summarize(
    total_belanja5 = sum(belanja5, na.rm = TRUE),
    total_peruntukan5 = sum(peruntukan5, na.rm = TRUE)) %>%
  mutate(total_belanja5_million = total_belanja5 / 1e6,
         total_peruntukan5_million = total_peruntukan5 /1e6)
ptj5 <- ggplot(rio_df_agihan_dan_belanja_ptj5 %>%
                 mutate(Belanja_oleh_NIH = (total_belanja5_million/total_peruntukan5_million)*100,
                        Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                 select(ptj,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
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
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 5", 
                               Baki_oleh_NIH = "Balance from Distribution 5"))

# Plot 6

rio_df_agihan_dan_belanja_ptj6 <- rio_df_agihan_dan_belanja %>%
  group_by(ptj) %>%
  summarize(
    total_belanja6 = sum(belanja6, na.rm = TRUE),
    total_peruntukan6 = sum(peruntukan6, na.rm = TRUE)) %>%
  mutate(total_belanja6_million = total_belanja6 / 1e6,
         total_peruntukan6_million = total_peruntukan6 /1e6)
ptj6 <- ggplot(rio_df_agihan_dan_belanja_ptj6 %>%
                 mutate(Belanja_oleh_NIH = (total_belanja6_million/total_peruntukan6_million)*100,
                        Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                 select(ptj,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
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
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 6", 
                               Baki_oleh_NIH = "Balance from Distribution 6"))

# Plot 7

rio_df_agihan_dan_belanja_ptj7 <- rio_df_agihan_dan_belanja %>%
  group_by(ptj) %>%
  summarize(
    total_belanja7 = sum(belanja7, na.rm = TRUE),
    total_peruntukan7 = sum(peruntukan7, na.rm = TRUE)) %>%
  mutate(total_belanja7_million = total_belanja7 / 1e6,
         total_peruntukan7_million = total_peruntukan7 /1e6)
ptj7 <- ggplot(rio_df_agihan_dan_belanja_ptj7 %>%
                 mutate(Belanja_oleh_NIH = (total_belanja7_million/total_peruntukan7_million)*100,
                        Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                 select(ptj,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
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
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 7", 
                               Baki_oleh_NIH = "Balance from Distribution 7"))

# Plot 8

rio_df_agihan_dan_belanja_ptj8 <- rio_df_agihan_dan_belanja %>%
  group_by(ptj) %>%
  summarize(
    total_belanja8 = sum(belanja8, na.rm = TRUE),
    total_peruntukan8 = sum(peruntukan8, na.rm = TRUE)) %>%
  mutate(total_belanja8_million = total_belanja8 / 1e6,
         total_peruntukan8_million = total_peruntukan8 /1e6)
ptj8 <- ggplot(rio_df_agihan_dan_belanja_ptj8 %>%
                 mutate(Belanja_oleh_NIH = (total_belanja8_million/total_peruntukan8_million)*100,
                        Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                 select(ptj,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
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
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 8", 
                               Baki_oleh_NIH = "Balance from Distribution 8"))

# Plot 9

rio_df_agihan_dan_belanja_ptj9 <- rio_df_agihan_dan_belanja %>%
  group_by(ptj) %>%
  summarize(
    total_belanja9 = sum(belanja9, na.rm = TRUE),
    total_peruntukan9 = sum(peruntukan9, na.rm = TRUE)) %>%
  mutate(total_belanja9_million = total_belanja9 / 1e6,
         total_peruntukan9_million = total_peruntukan9 /1e6)
ptj9 <- ggplot(rio_df_agihan_dan_belanja_ptj9 %>%
                 mutate(Belanja_oleh_NIH = (total_belanja9_million/total_peruntukan9_million)*100,
                        Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                 select(ptj,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
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
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 9", 
                               Baki_oleh_NIH = "Balance from Distribution 9"))

# Plot 10

rio_df_agihan_dan_belanja_ptj10 <- rio_df_agihan_dan_belanja %>%
  group_by(ptj) %>%
  summarize(
    total_belanja10 = sum(belanja10, na.rm = TRUE),
    total_peruntukan10 = sum(peruntukan10, na.rm = TRUE)) %>%
  mutate(total_belanja10_million = total_belanja10 / 1e6,
         total_peruntukan10_million = total_peruntukan10 /1e6)
ptj10 <- ggplot(rio_df_agihan_dan_belanja_ptj10 %>%
                  mutate(Belanja_oleh_NIH = (total_belanja10_million/total_peruntukan10_million)*100,
                         Baki_oleh_NIH = 100 - Belanja_oleh_NIH) %>%
                  select(ptj,Belanja_oleh_NIH,Baki_oleh_NIH ) %>%
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
  ggtitle("") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_oleh_NIH" = "#C1D7C1","Baki_oleh_NIH" = "#DDB8A6"),
                    labels = c(Belanja_oleh_NIH = "Spending from Distribution 10", 
                               Baki_oleh_NIH = "Balance from Distribution 10"))


