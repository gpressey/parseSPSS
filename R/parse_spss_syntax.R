# The following is a proof-of-concept script, using the 1976 Census Individuals PUMF file.

# It currently works. However, it's fragile and inflexible, and needs continuing work.

library(tidyverse)
library(parseSPSS)
library(skimr)

df_info <- read_sps_fwf(
  syntax = here::here("data","indiv76_eng.sps"),
  filename = here::here("data", "indiv76.txt"))

df <- df_info$data
df_dict <- df_info$dictionary

skim(df)

df %>%
  mutate(age = as.numeric(as.character(age))) %>%
  filter(age > 14) %>%
  ggplot(aes(x = age, fill = dgree)) +
  geom_bar(position = "fill", width = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(labels = function(x){str_wrap(x, width = 25)}) +
  #facet_wrap(~ dgree, ncol = 2) +
  labs(
    title = "Educational attainment by age, Canada, 1976",
    caption = "Source: StatCan, Census of Canada 1976 PUMF",
    fill = "",
    x = "Age",
    y = ""
  ) +
  theme(legend.position = "bottom")

ggsave(here::here("charts","c1976ind_educ_age.png"))

df %>%
  mutate(age = as.numeric(as.character(age))) %>%
  filter(age > 14) %>%
  ggplot(aes(x = age, fill = mob5)) +
  geom_bar(position = "fill", width = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(labels = function(x){str_wrap(x, width = 25)}) +
  #facet_wrap(~ dgree, ncol = 2) +
  labs(
    title = "Mobility by age, Canada, 1976",
    caption = "Source: StatCan, Census of Canada 1976 PUMF",
    fill = "",
    x = "Age",
    y = ""
  ) +
  theme(legend.position = "bottom")

ggsave(here::here("charts","c1976_ind_mob_age.png"))

