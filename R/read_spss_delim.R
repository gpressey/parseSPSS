
get_commands <- function(spss_syntax){
  spss_syntax %>%
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
    select(-end_command, -new_command) %>%
    # now, we'll fill in the missing values so we can nest the files
    fill(type, .direction = "down") %>%
    nest(data = -type)

}

read_sps_fwf <- function(syntax, filename){

  # load the syntax file
  df_syntax <-
    read_delim(
      file = syntax,
      delim = "\n",
      col_names = "command")

  all_commands <- get_commands(df_syntax)

  # extract the file widths
  file_details <- all_commands %>%
    filter(type == "delim") %>%
    mutate(
      do = map(data, ~get_spss_delims(.x$command))) %>%
    select(do) %>%
    unnest(do)

  # read in the text file
  df_census <- read_fwf(
    file = filename,
    col_positions = file_details) %>%
    mutate(
      # make everything numeric
      # TO DO: if string variables I'll need to find a way of dealing with that
      across(everything(), as.numeric)
    )

  # extract the variable labels
  variable_labels <- df_syntax %>%
    filter(type == "variable_label") %>%
    mutate(do = map(data, ~get_spss_variable_labels(.x$command))) %>%
    select(do) %>%
    unnest(do)

  #print(variable_labels)

  # extract the value labels
  value_labels <- df_syntax %>%
    filter(type == "value_label") %>%
    mutate(do = map(data, ~get_spss_value_labels(.x$command))) %>%
    select(do) %>%
    unnest(do)

  # join to create the competed dictionary
  df_dictionary <- left_join(variable_labels, value_labels)
  rm(value_labels, variable_labels)

  # now, add the value labels back on to the main text file as factors
  # taking each column name
  df <- map_dfc(names(df_census), ~{

    # first, extract the labels from the dictionary
    df_labels <- df_dictionary %>%
      filter(col_name == .x)

    # the numeric vector we're making into a factor
    vector <- df_census[[.x]]

    # not all elements of the vector will be labelled. If an element is present
    # without a name, the factor name is numeric value
    df_names <-
      tibble(
        # take all the unique values
        level = unique(vector)
      ) %>%
      # sorting should put the factor labels in the correct order
      arrange(level) %>%
      # join the character labels to the numeric levels
      left_join(df_labels, by = c("level" = "col_name_value")) %>%
      # if there's no character label the value should be that of the level
      mutate(label = if_else(is.na(label), as.character(level), label))

    levels <- df_names$level
    labels <- df_names$label

    df_census[[.x]] <- factor(x = vector, levels = levels, labels = labels)
  })

  names(df) <- names(df_census)

  return(list(data = df, dictionary = df_dictionary))
}
