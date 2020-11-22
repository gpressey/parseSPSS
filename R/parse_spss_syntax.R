# First, let's take a look at what we have to do and add some sample
# data into the project directory

# the following code more or less parses a syntax file, loads a fwf, and
# reads in value and variable labels. However, it's fragile and inflexible.

library(tidyverse)

df_info <- read_sps_fwf(
  syntax = here::here("data","indiv76_eng.sps"),
  filename = here::here("data", "indiv76.txt"))

df <- df_info$data
df_dict <- df_info$dictionary

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
  group_by(age, var = usmarst) %>%
  tally() %>%
  ggplot() +
  geom_col(
    aes(x = as.numeric(age), y = n, fill = var),
    #position = "fill",
    width = 1)

df %>%
  group_by(age, var = usmarst) %>%
  tally() %>%
  mutate(age = as.numeric(as.character(age))) %>%
  arrange(age) %>%
  ggplot(aes(x = age, y = n, fill = var)) +
  geom_col(alpha = 1/2, position = "stack", width = 1) +
  theme(legend.position = "bottom")

check <- df %>%
  group_by(age, var = usmarst) %>%
  tally() %>%
  mutate(age = as.numeric(as.character(age))) %>%
  arrange(age) %>%
  pivot_wider(names_from = var, values_from = n)

df %>%
  mutate(
    age = as.numeric(as.character(age))
  ) %>%
  ggplot() +
  geom_density(
    aes(x = age, fill = lftag),
    adjust = 1,
    #width = 1,
    position = "fill") +
  facet_wrap(~sex, ncol = 1) +
  theme(legend.position = "bottom")
