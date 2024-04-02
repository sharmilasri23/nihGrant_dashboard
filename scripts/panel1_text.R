#load packages
#load packages
pacman::p_load(
  tidyverse
)

#summarise cards
summary_cards <- rio_df_simple_panel %>%
  summarise(peruntukan = sum (peruntukan, na.rm=T),
            belanja = sum(belanja, na.rm=T),
            baki =  sum(baki, na.rm=T))
