read_sps_delim <- function(syntax, filename){

  df_syntax <-
    read_delim(
      file = syntax,
      delim = "\n",
      col_names = "command") %>%
    mutate(
      #command = stringi::stri_escape_unicode(command),
      command = str_squish(command) %>% tolower(),
      end_command = str_detect(command, "\\.$"),
      new_command = lag(end_command, default = T),
      type = case_when(
        str_detect(command,"data list file") ~ "delim",
        str_detect(command,"variable label") ~ "variable_label",
        str_detect(command,"value label") ~ "value_label",
        str_detect(command,"missing value") ~ "missing_value",
        new_command ~ "not_implemented"
      )
    ) %>%
    fill(type, .direction = "down") %>%
    nest(data = -type)

  file_details <- df_syntax %>%
    filter(type == "delim") %>%
    mutate(
      do = map(data, get_spss_delims)) %>%
    select(do) %>%
    unnest(do)

  df_census <- read_fwf(
    file = filename,
    col_positions = file_details) %>%
    mutate(
      across(everything(), as.numeric)
    )

  variable_labels <- df_syntax %>%
    filter(type == "variable_label") %>%
    mutate(do = map(data, get_spss_variable_labels)) %>%
    select(do) %>%
    unnest(do)

  value_labels <- df_syntax %>%
    filter(type == "value_label") %>%
    mutate(do = map(data, get_spss_value_labels)) %>%
    select(do) %>%
    unnest(do)

  df_dictionary <- left_join(variable_labels, value_labels)

  rm(value_labels, variable_labels)

  df <- map_dfc(names(df_census), ~{

    df_labels <- df_dictionary %>%
      filter(col_name == .x) %>%
      select(value_labels) %>% unnest(value_labels)

    vector <- df_census[[.x]]

    df_names <- tibble(
      level = df_census[[.x]] %>% unique()
    ) %>%
      left_join(df_labels) %>%
      mutate(label = if_else(is.na(label), as.character(level), label))

    levels <- df_names$level
    labels <- df_names$label

    df_census[[.x]] <- factor(x = vector, levels = levels, labels = labels)
  })

  names(df) <- names(df_census)

  return(list(data = df, dictionary = df_dictionary))
}
