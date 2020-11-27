library(tidyverse)

df_syntax <-
  read_delim(
    file = here::here("data", "indiv76_eng.sps"),
    delim = "\n",
    col_names = "command") %>%
  mutate(
    # breaking out each command.
    #command = stringi::stri_escape_unicode(command),
    command = str_squish(command) %>% tolower(),
    end_command = str_detect(command, "\\.$"),
    new_command = lag(end_command, default = T),
    type = case_when(
      str_detect(command,"data list file") ~ "delim",
      str_detect(command,"variable label") ~ "variable_label",
      str_detect(command,"value label") ~ "value_label",
      #str_detect(command,"missing value") ~ "missing_value",
      new_command ~ "not_implemented"
    )
  ) %>%
  # now, we'll fill in the missing values so we can nest the files
  fill(type, .direction = "down") %>%
  nest(data = -type)

look <- 1

test <- df_syntax %>%
  filter(type == "value_label") %>%
  unnest(data) %>%
  select(command)

test <- test$command

get_spss_value_labels(test)
