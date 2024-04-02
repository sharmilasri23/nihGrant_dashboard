
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
  labs(x = "Million (MYR)", y = "Research Grand Cluster", fill = "") +
  theme(legend.position = "top") +
  ggtitle("Agihan Peruntukan oleh Kerajaan dan NIH") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Peruntukan_dari_Kerajaan" = "#5fa8d3", "Peruntukan_dari_NIH" = "#3c8a99", "Baki_dari_NIH" = "#64b9b9"),
                    labels = c(Peruntukan_dari_Kerajaan = "Peruntukan dari Kerajaan", Peruntukan_dari_NIH = "Peruntukan dari NIH", Baki_dari_NIH = "Baki dari NIH"))


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
  ggtitle("Agihan Peruntukan oleh Kerajaan dan NIH (%)") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Peratus_peruntukan" = "#5fa8d3", "Peratus_baki" = "#3c8a99", "Peratus_belanja" = "#64b9b9"),
                    labels = c(Peratus_peruntukan = "Peratus peruntukan", Peratus_baki = "Peratus baki", Peratus_belanja = "Peratus belanja"))

## Peruntukan dan belanja by PTJ
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
  ggtitle("Belanja dan Baki daripada NIH (MYR)") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_dari_NIH" = "#64b9b9","Baki_dari_NIH" = "#3c8a99"),
                    labels = c(Belanja_dari_NIH = "Belanja", Baki_dari_NIH = "Baki"))


#Peruntukan and belanja by PTJ (%)
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
  ggtitle("Belanja dan Baki daripada NIH (%)") +
  theme(legend.position = "top",
        plot.title = element_text(color = "black", face = "bold")) +
  scale_fill_manual(values = c("Belanja_dari_NIH" = "#64b9b9","Baki_dari_NIH" = "#3c8a99"),
                    labels = c(Belanja_dari_NIH = "Belanja", Baki_dari_NIH = "Baki"))

## 2nd page (Agihan dan Belanja corresponding plots)

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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 1", 
                               Baki_oleh_NIH = "Baki daripada Agihan 1"))



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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 2", 
                               Baki_oleh_NIH = "Baki daripada Agihan 2"))



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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 3", 
                               Baki_oleh_NIH = "Baki daripada Agihan 3"))

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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 4", 
                               Baki_oleh_NIH = "Baki daripada Agihan 4"))


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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 5", 
                               Baki_oleh_NIH = "Baki daripada Agihan 5"))


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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 6", 
                               Baki_oleh_NIH = "Baki daripada Agihan 6"))


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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 7", 
                               Baki_oleh_NIH = "Baki daripada Agihan 7"))


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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 8", 
                               Baki_oleh_NIH = "Baki daripada Agihan 8"))


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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 9", 
                               Baki_oleh_NIH = "Baki daripada Agihan 9"))


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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 10", 
                               Baki_oleh_NIH = "Baki daripada Agihan 10"))





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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 1", 
                               Baki_oleh_NIH = "Baki daripada Agihan 1"))

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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 2", 
                               Baki_oleh_NIH = "Baki daripada Agihan 2"))


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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 3", 
                               Baki_oleh_NIH = "Baki daripada Agihan 3"))

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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 4", 
                               Baki_oleh_NIH = "Baki daripada Agihan 4"))

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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 5", 
                               Baki_oleh_NIH = "Baki daripada Agihan 5"))

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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 6", 
                               Baki_oleh_NIH = "Baki daripada Agihan 6"))

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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 7", 
                               Baki_oleh_NIH = "Baki daripada Agihan 7"))

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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 8", 
                               Baki_oleh_NIH = "Baki daripada Agihan 8"))

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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 9", 
                               Baki_oleh_NIH = "Baki daripada Agihan 9"))

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
                    labels = c(Belanja_oleh_NIH = "Belanja daripada Agihan 10", 
                               Baki_oleh_NIH = "Baki daripada Agihan 10"))
