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


#Flextable
rio_df_agihan_dan_belanja_cluster <- rio_df_agihan_dan_belanja %>%
  group_by(kluster) %>%
  summarize(
    total_belanja1 = sum(belanja1, na.rm = TRUE),
    total_peruntukan1 = sum(peruntukan1, na.rm = TRUE),
    total_belanja2 = sum(belanja2, na.rm = TRUE),
    total_peruntukan2 = sum(peruntukan2, na.rm = TRUE),
    total_belanja3 = sum(belanja3, na.rm = TRUE),
    total_peruntukan3 = sum(peruntukan3, na.rm = TRUE),
    total_belanja4 = sum(belanja4, na.rm = TRUE),
    total_peruntukan4 = sum(peruntukan4, na.rm = TRUE),
    total_belanja5 = sum(belanja5, na.rm = TRUE),
    total_peruntukan5 = sum(peruntukan5, na.rm = TRUE),
    total_belanja6 = sum(belanja6, na.rm = TRUE),
    total_peruntukan6 = sum(peruntukan6, na.rm = TRUE),
    total_belanja7 = sum(belanja7, na.rm = TRUE),
    total_peruntukan7 = sum(peruntukan7, na.rm = TRUE),
    total_belanja8 = sum(belanja8, na.rm = TRUE),
    total_peruntukan8 = sum(peruntukan8, na.rm = TRUE),
    total_belanja9 = sum(belanja9, na.rm = TRUE),
    total_peruntukan9 = sum(peruntukan9, na.rm = TRUE),
    total_belanja10 = sum(belanja10, na.rm = TRUE),
    total_peruntukan10 = sum(peruntukan10, na.rm = TRUE)
  )


#change datatype

change_numeric <- rio_df_agihan_dan_belanja_cluster %>%
  mutate_at(vars(total_belanja1, total_peruntukan1, 
                 total_belanja2, total_peruntukan2,
                 total_belanja3, total_peruntukan3,
                 total_belanja4, total_peruntukan4,
                 total_belanja5, total_peruntukan5,
                 total_belanja6, total_peruntukan6,
                 total_belanja7, total_peruntukan7,
                 total_belanja8, total_peruntukan8,
                 total_belanja9, total_peruntukan9,
                 total_belanja10, total_peruntukan10),
            list(~ as.integer(.))) 

# Sub tables (summarize_table1) --------------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_cluster %>%
  select(kluster, total_belanja1, total_peruntukan1) %>%
  mutate(total_belanja1 = total_peruntukan1 - total_belanja1)

summarize_table1 <- flextable(summarize_table) 
summarize_table1 <- summarize_table1 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    kluster = "Cluster", 
    total_belanja1 = "Spending 1",                  
    total_peruntukan1 = "Allocation 1")

border_style = officer::fp_border(color="black", width=1)
summarize_table1 <- summarize_table1 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

summarize_table1 <- summarize_table1 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

summarize_table1 <- summarize_table1 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

summarize_table1 <- summarize_table1 %>%
  bg(part = "header", bg = "#CCCCCC")

summarize_table1 <- summarize_table1 %>%
  bg(j = 2, i = ~ total_belanja1 > total_peruntukan1, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan1 > total_belanja1, part = "body", bg = "#C1D7C1")



# summarize_table2 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_cluster %>%
  select(kluster, total_belanja2, total_peruntukan2) %>%
  mutate(total_belanja2 = total_peruntukan2 - total_belanja2)

summarize_table2 <- flextable(summarize_table) 
summarize_table2 <- summarize_table2 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    kluster = "Cluster", 
    total_belanja2 = "Spending 2",                  
    total_peruntukan2 = "Allocation 2")

border_style = officer::fp_border(color="black", width=1)
summarize_table2 <- summarize_table2 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

summarize_table2 <- summarize_table2 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

summarize_table2 <- summarize_table2 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

summarize_table2 <- summarize_table2 %>%
  bg(part = "header", bg = "#CCCCCC")

summarize_table2 <- summarize_table2 %>%
  bg(j = 2, i = ~ total_belanja2 < total_peruntukan2, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan2 > total_belanja2, part = "body", bg = "#C1D7C1")



# summarize_table3 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_cluster %>%
  select(kluster, total_belanja3, total_peruntukan3) %>%
  mutate(total_belanja3 = total_peruntukan3 - total_belanja3)

summarize_table3 <- flextable(summarize_table) 
summarize_table3 <- summarize_table3 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    kluster = "Cluster", 
    total_belanja3 = "Spending 3",                  
    total_peruntukan3 = "Allocation 3")

border_style = officer::fp_border(color="black", width=1)
summarize_table3 <- summarize_table3 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

summarize_table3 <- summarize_table3 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

summarize_table3 <- summarize_table3 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

summarize_table3 <- summarize_table3 %>%
  bg(part = "header", bg = "#CCCCCC")

summarize_table3 <- summarize_table3 %>%
  bg(j = 2, i = ~ total_belanja3 < total_peruntukan3, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan3 > total_belanja3, part = "body", bg = "#C1D7C1")


# summarize_table4 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_cluster %>%
  select(kluster, total_belanja4, total_peruntukan4) %>%
  mutate(total_belanja4 = total_peruntukan4 - total_belanja4)

summarize_table4 <- flextable(summarize_table) 
summarize_table4 <- summarize_table4 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    kluster = "Cluster", 
    total_belanja4 = "Spending 4",                  
    total_peruntukan4 = "Allocation 4")

border_style = officer::fp_border(color="black", width=1)
summarize_table4 <- summarize_table4 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

summarize_table4 <- summarize_table4 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

summarize_table4 <- summarize_table4 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

summarize_table4 <- summarize_table4 %>%
  bg(part = "header", bg = "#CCCCCC")

summarize_table4 <- summarize_table4 %>%
  bg(j = 2, i = ~ total_belanja4 < total_peruntukan4, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan4 > total_belanja4, part = "body", bg = "#C1D7C1")



# summarize_table5 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_cluster %>%
  select(kluster, total_belanja5, total_peruntukan5) %>%
  mutate(total_belanja5 = total_peruntukan5 - total_belanja5)

summarize_table5 <- flextable(summarize_table) 
summarize_table5 <- summarize_table5 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    kluster = "Cluster", 
    total_belanja5 = "Spending 5",                  
    total_peruntukan5 = "Allocation 5")

border_style = officer::fp_border(color="black", width=1)
summarize_table5 <- summarize_table5 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

summarize_table5 <- summarize_table5 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

summarize_table5 <- summarize_table5 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

summarize_table5 <- summarize_table5 %>%
  bg(part = "header", bg = "#CCCCCC")

summarize_table5 <- summarize_table5 %>%
  bg(j = 2, i = ~ total_belanja5 < total_peruntukan5, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan5 > total_belanja5, part = "body", bg = "#C1D7C1")



# summarize_table6 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_cluster %>%
  select(kluster, total_belanja6, total_peruntukan6) %>%
  mutate(total_belanja6 = total_peruntukan6 - total_belanja6)

summarize_table6 <- flextable(summarize_table) 
summarize_table6 <- summarize_table6 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    kluster = "Cluster", 
    total_belanja6 = "Spending 6",                  
    total_peruntukan6 = "Allocation 6")

border_style = officer::fp_border(color="black", width=1)
summarize_table6 <- summarize_table6 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

summarize_table6 <- summarize_table6 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

summarize_table6 <- summarize_table6 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

summarize_table6 <- summarize_table6 %>%
  bg(part = "header", bg = "#CCCCCC")

summarize_table6 <- summarize_table6 %>%
  bg(j = 2, i = ~ total_belanja6 < total_peruntukan6, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan6 > total_belanja6, part = "body", bg = "#C1D7C1")


# summarize_table7 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_cluster %>%
  select(kluster, total_belanja7, total_peruntukan7) %>%
  mutate(total_belanja7 = total_peruntukan7 - total_belanja7)

summarize_table7 <- flextable(summarize_table) 
summarize_table7 <- summarize_table7 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    kluster = "Cluster", 
    total_belanja7 = "Spending 7",                  
    total_peruntukan7 = "Allocation 7")

border_style = officer::fp_border(color="black", width=1)
summarize_table7 <- summarize_table7 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

summarize_table7 <- summarize_table7 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

summarize_table7 <- summarize_table7 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

summarize_table7 <- summarize_table7 %>%
  bg(part = "header", bg = "#CCCCCC")

summarize_table7 <- summarize_table7 %>%
  bg(j = 2, i = ~ total_belanja7 < total_peruntukan7, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan7 > total_belanja7, part = "body", bg = "#C1D7C1")


# summarize_table8 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_cluster %>%
  select(kluster, total_belanja8, total_peruntukan8) %>%
  mutate(total_belanja8 = total_peruntukan8 - total_belanja8)

summarize_table8 <- flextable(summarize_table) 
summarize_table8 <- summarize_table8 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    kluster = "Cluster", 
    total_belanja8 = "Spending 8",                  
    total_peruntukan8 = "Allocation 8")

border_style = officer::fp_border(color="black", width=1)
summarize_table8 <- summarize_table8 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

summarize_table8 <- summarize_table8 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

summarize_table8 <- summarize_table8 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

summarize_table8 <- summarize_table8 %>%
  bg(part = "header", bg = "#CCCCCC")

summarize_table8 <- summarize_table8 %>%
  bg(j = 2, i = ~ total_belanja8 < total_peruntukan8, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan8 > total_belanja8, part = "body", bg = "#C1D7C1")


# summarize_table9 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_cluster %>%
  select(kluster, total_belanja9, total_peruntukan9) %>%
  mutate(total_belanja9 = total_peruntukan9 - total_belanja9)

summarize_table9 <- flextable(summarize_table) 
summarize_table9 <- summarize_table9 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    kluster = "Cluster", 
    total_belanja9 = "Spending 9",                  
    total_peruntukan9 = "Allocation 9")

border_style = officer::fp_border(color="black", width=1)
summarize_table9 <- summarize_table9 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

summarize_table9 <- summarize_table9 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

summarize_table9 <- summarize_table9 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

summarize_table9 <- summarize_table9 %>%
  bg(part = "header", bg = "#CCCCCC")

summarize_table9 <- summarize_table9 %>%
  bg(j = 2, i = ~ total_belanja9 < total_peruntukan9, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan9 > total_belanja9, part = "body", bg = "#C1D7C1")


# summarize_table10 -------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_cluster %>%
  select(kluster, total_belanja10, total_peruntukan10) %>%
  mutate(total_belanja10 = total_peruntukan10 - total_belanja10)

summarize_table10 <- flextable(summarize_table) 
summarize_table10 <- summarize_table10 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    kluster = "Cluster", 
    total_belanja10 = "Spending 10",                  
    total_peruntukan10 = "Allocation 10")

border_style = officer::fp_border(color="black", width=1)
summarize_table10 <- summarize_table10 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

summarize_table10 <- summarize_table10 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

summarize_table10 <- summarize_table10 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

summarize_table10 <- summarize_table10 %>%
  bg(part = "header", bg = "#CCCCCC")

summarize_table10 <- summarize_table10 %>%
  bg(j = 2, i = ~ total_belanja10 < total_peruntukan10, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan10 > total_belanja10, part = "body", bg = "#C1D7C1")






# PTJ Tables --------------------------------------------------------------

rio_df_agihan_dan_belanja_ptj <- rio_df_agihan_dan_belanja %>%
  group_by(ptj) %>%
  summarize(
    total_belanja1 = sum(belanja1, na.rm = TRUE),
    total_peruntukan1 = sum(peruntukan1, na.rm = TRUE),
    total_belanja2 = sum(belanja2, na.rm = TRUE),
    total_peruntukan2 = sum(peruntukan2, na.rm = TRUE),
    total_belanja3 = sum(belanja3, na.rm = TRUE),
    total_peruntukan3 = sum(peruntukan3, na.rm = TRUE),
    total_belanja4 = sum(belanja4, na.rm = TRUE),
    total_peruntukan4 = sum(peruntukan4, na.rm = TRUE),
    total_belanja5 = sum(belanja5, na.rm = TRUE),
    total_peruntukan5 = sum(peruntukan5, na.rm = TRUE),
    total_belanja6 = sum(belanja6, na.rm = TRUE),
    total_peruntukan6 = sum(peruntukan6, na.rm = TRUE),
    total_belanja7 = sum(belanja7, na.rm = TRUE),
    total_peruntukan7 = sum(peruntukan7, na.rm = TRUE),
    total_belanja8 = sum(belanja8, na.rm = TRUE),
    total_peruntukan8 = sum(peruntukan8, na.rm = TRUE),
    total_belanja9 = sum(belanja9, na.rm = TRUE),
    total_peruntukan9 = sum(peruntukan9, na.rm = TRUE),
    total_belanja10 = sum(belanja10, na.rm = TRUE),
    total_peruntukan10 = sum(peruntukan10, na.rm = TRUE)
  )


# ptj table 1 -------------------------------------------------------------


summarize_table <- rio_df_agihan_dan_belanja_ptj %>%
  select(ptj, total_belanja1, total_peruntukan1) %>%
  mutate(total_belanja1 = total_peruntukan1 - total_belanja1)

ptj_table1 <- flextable(summarize_table) 
ptj_table1 <- ptj_table1 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    ptj = "PTJ", 
    total_belanja1 = "Spending 1",                  
    total_peruntukan1 = "Allocation 1")

border_style = officer::fp_border(color="black", width=1)
ptj_table1 <- ptj_table1 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

ptj_table1 <- ptj_table1 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

ptj_table1 <- ptj_table1 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

ptj_table1 <- ptj_table1 %>%
  bg(part = "header", bg = "#CCCCCC")

ptj_table1 <- ptj_table1 %>%
  bg(j = 2, i = ~ total_belanja1 < total_peruntukan1, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan1 > total_belanja1, part = "body", bg = "#C1D7C1")


# ptj table 2 -------------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_ptj %>%
  select(ptj, total_belanja2, total_peruntukan2) %>%
  mutate(total_belanja2 = total_peruntukan2 - total_belanja2)

ptj_table2 <- flextable(summarize_table) 
ptj_table2 <- ptj_table2 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    ptj = "PTJ", 
    total_belanja2 = "Spending 2",                  
    total_peruntukan2 = "Allocation 2")

border_style = officer::fp_border(color="black", width=1)
ptj_table2 <- ptj_table2 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

ptj_table2 <- ptj_table2 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

ptj_table2 <- ptj_table2 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

ptj_table2 <- ptj_table2 %>%
  bg(part = "header", bg = "#CCCCCC")

ptj_table2 <- ptj_table2 %>%
  bg(j = 2, i = ~ total_belanja2 < total_peruntukan2, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan2 > total_belanja2, part = "body", bg = "#C1D7C1")


# ptj table 3 -------------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_ptj %>%
  select(ptj, total_belanja3, total_peruntukan3) %>%
  mutate(total_belanja3 = total_peruntukan3 - total_belanja3)

ptj_table3 <- flextable(summarize_table) 
ptj_table3 <- ptj_table3 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    ptj = "PTJ", 
    total_belanja3 = "Spending 3",                  
    total_peruntukan3 = "Allocation 3")

border_style = officer::fp_border(color="black", width=1)
ptj_table3 <- ptj_table3 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

ptj_table3 <- ptj_table3 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

ptj_table3 <- ptj_table3 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

ptj_table3 <- ptj_table3 %>%
  bg(part = "header", bg = "#CCCCCC")

ptj_table3 <- ptj_table3 %>%
  bg(j = 2, i = ~ total_belanja3 < total_peruntukan3, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan3 > total_belanja3, part = "body", bg = "#C1D7C1")


# ptj table 4 -------------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_ptj %>%
  select(ptj, total_belanja4, total_peruntukan4) %>%
  mutate(total_belanja4 = total_peruntukan4 - total_belanja4)

ptj_table4 <- flextable(summarize_table) 
ptj_table4 <- ptj_table4 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    ptj = "PTJ", 
    total_belanja4 = "Spending 4",                  
    total_peruntukan4 = "Allocation 4")

border_style = officer::fp_border(color="black", width=1)
ptj_table4 <- ptj_table4 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

ptj_table4 <- ptj_table4 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

ptj_table4 <- ptj_table4 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

ptj_table4 <- ptj_table4 %>%
  bg(part = "header", bg = "#CCCCCC")

ptj_table4 <- ptj_table4 %>%
  bg(j = 2, i = ~ total_belanja4 < total_peruntukan4, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan4 > total_belanja4, part = "body", bg = "#C1D7C1")


# ptj table 5 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_ptj %>%
  select(ptj, total_belanja5, total_peruntukan5) %>%
  mutate(total_belanja5 = total_peruntukan5 - total_belanja5)

ptj_table5 <- flextable(summarize_table) 
ptj_table5 <- ptj_table5 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    ptj = "PTJ", 
    total_belanja5 = "Spending 5",                  
    total_peruntukan5 = "Allocation 5")

border_style = officer::fp_border(color="black", width=1)
ptj_table5 <- ptj_table5 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

ptj_table5 <- ptj_table5 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

ptj_table5 <- ptj_table5 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

ptj_table5 <- ptj_table5 %>%
  bg(part = "header", bg = "#CCCCCC")

ptj_table5 <- ptj_table5 %>%
  bg(j = 2, i = ~ total_belanja5 < total_peruntukan5, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan5 > total_belanja5, part = "body", bg = "#C1D7C1")



# ptj table 6 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_ptj %>%
  select(ptj, total_belanja6, total_peruntukan6) %>%
  mutate(total_belanja6 = total_peruntukan6 - total_belanja6)

ptj_table6 <- flextable(summarize_table) 
ptj_table6 <- ptj_table6 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    ptj = "PTJ", 
    total_belanja6 = "Spending 6",                  
    total_peruntukan6 = "Allocation 6")

border_style = officer::fp_border(color="black", width=1)
ptj_table6 <- ptj_table6 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

ptj_table6 <- ptj_table6 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

ptj_table6 <- ptj_table6 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

ptj_table6 <- ptj_table6 %>%
  bg(part = "header", bg = "#CCCCCC")

ptj_table6 <- ptj_table6 %>%
  bg(j = 2, i = ~ total_belanja6 < total_peruntukan6, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan6 > total_belanja6, part = "body", bg = "#C1D7C1")


# ptj table 7 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_ptj %>%
  select(ptj, total_belanja7, total_peruntukan7) %>%
  mutate(total_belanja7 = total_peruntukan7 - total_belanja7)

ptj_table7 <- flextable(summarize_table) 
ptj_table7 <- ptj_table7 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    ptj = "PTJ", 
    total_belanja7 = "Spending 7",                  
    total_peruntukan7 = "Allocation 7")

border_style = officer::fp_border(color="black", width=1)
ptj_table7 <- ptj_table7 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

ptj_table7 <- ptj_table7 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

ptj_table7 <- ptj_table7 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

ptj_table7 <- ptj_table7 %>%
  bg(part = "header", bg = "#CCCCCC")

ptj_table7 <- ptj_table7 %>%
  bg(j = 2, i = ~ total_belanja7 < total_peruntukan7, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan7 > total_belanja7, part = "body", bg = "#C1D7C1")


# ptj table 8 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_ptj %>%
  select(ptj, total_belanja8, total_peruntukan8) %>%
  mutate(total_belanja8 = total_peruntukan8 - total_belanja8)

ptj_table8 <- flextable(summarize_table) 
ptj_table8 <- ptj_table8 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    ptj = "PTJ", 
    total_belanja8 = "Spending 8",                  
    total_peruntukan8 = "Allocation 8")

border_style = officer::fp_border(color="black", width=1)
ptj_table8 <- ptj_table8 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

ptj_table8 <- ptj_table8 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

ptj_table8 <- ptj_table8 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

ptj_table8 <- ptj_table8 %>%
  bg(part = "header", bg = "#CCCCCC")

ptj_table8 <- ptj_table8 %>%
  bg(j = 2, i = ~ total_belanja8 < total_peruntukan8, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan8 > total_belanja8, part = "body", bg = "#C1D7C1")


# ptj table 9 --------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_ptj %>%
  select(ptj, total_belanja9, total_peruntukan9) %>%
  mutate(total_belanja9 = total_peruntukan9 - total_belanja9)

ptj_table9 <- flextable(summarize_table) 
ptj_table9 <- ptj_table9 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    ptj = "PTJ", 
    total_belanja9 = "Spending 9",                  
    total_peruntukan9 = "Allocation 9")

border_style = officer::fp_border(color="black", width=1)
ptj_table9 <- ptj_table9 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

ptj_table9 <- ptj_table9 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

ptj_table9 <- ptj_table9 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

ptj_table9 <- ptj_table9 %>%
  bg(part = "header", bg = "#CCCCCC")

ptj_table9 <- ptj_table9 %>%
  bg(j = 2, i = ~ total_belanja9 < total_peruntukan9, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan9 > total_belanja9, part = "body", bg = "#C1D7C1")


# ptj table 10 -------------------------------------------------------

summarize_table <- rio_df_agihan_dan_belanja_ptj %>%
  select(ptj, total_belanja10, total_peruntukan10) %>%
  mutate(total_belanja10 = total_peruntukan10 - total_belanja10)

ptj_table10 <- flextable(summarize_table) 
ptj_table10 <- ptj_table10 %>%
  set_table_properties(width = .75, layout = "autofit") %>%
  align(align = "center", part = "all") %>%
  set_header_labels(        
    ptj = "PTJ", 
    total_belanja10 = "Spending 10",                  
    total_peruntukan10 = "Allocation 10")


border_style = officer::fp_border(color="black", width=1)
ptj_table10 <- ptj_table10 %>%
  border_remove() %>%
  theme_booktabs() %>%
  vline(part = "all", j = 1, border = border_style) %>%    
  vline(part = "all", j = 2, border = border_style) %>%      
  vline(part = "all", j = 3, border = border_style) %>%
  vline_left(part = "all", border = border_style)

ptj_table10 <- ptj_table10 %>%
  set_caption(
    as_paragraph(
      as_chunk("", 
               props = fp_text_default(font.family = "Times New Roman",
                                       font.size = 14, bold = TRUE))
    ), word_stylename = "Table Caption"
  )

ptj_table10 <- ptj_table10 %>%
  flextable::align(align = "center", j = c(1:3), part = "all")

ptj_table10 <- ptj_table10 %>%
  bg(part = "header", bg = "#CCCCCC")

ptj_table10 <- ptj_table10 %>%
  bg(j = 2, i = ~ total_belanja10 < total_peruntukan10, part = "body", bg = "#DDB8A6") %>%
  bg(j = 3, i = ~ total_peruntukan10 > total_belanja10, part = "body", bg = "#C1D7C1")


