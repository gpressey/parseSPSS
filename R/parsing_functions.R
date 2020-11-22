#' Get SPSS Text File Delimitors
#'
#' @param df A data file consisting of a complete 'data file list' command to read a fixed width text file
#'
#' @return A fwf_positions object
#' @export
#'
#' @examples
get_spss_delims <- function(df){

  df <- df %>%
    # to do:
    # ---make sure that the width definitions start on the next line
    # ---make sure that only one width definition is on each line
    filter(!new_command) %>%
    mutate(
      # each definition is a col_name, a start position, and a end position
      command = command %>% str_replace_all("-|\\."," ") %>% str_squish()
    ) %>%
    separate(command, into = c("col_names","start","end"), sep = " ") %>%
    select(start, end, col_names) %>%
    mutate(
      # make sure formats are okay
      start = as.numeric(start),
      end = as.numeric(end),
      col_names = as.character(col_names)
    )

  fwf_positions(start = df$start, end = df$end, col_names = df$col_names)
}


get_spss_variable_labels <- function(df){
  df %>%
    # to do:
    # ---make sure that the definitions begin on a new line
    # ---make sure that only one definition is on each line
    filter(!new_command) %>%
    mutate(
      command = str_squish(command) %>% str_remove("\\.$")) %>%
    separate(command, into = c("col_name","col_label"), sep = " ", extra = "merge") %>%
    select(col_name, col_label) %>%
    mutate(
      # remove the quotes from the start and end of label
      col_label = str_remove_all(col_label, "^\'|^\"|\'$|\"")
    )
}

get_spss_value_labels <- function(df){
  df <- df %>%
    filter(!new_command) %>%
    select(-end_command, -new_command) %>%
    mutate(
      # get rid of trailing syntax like `.` and `/`
      command = str_squish(command) %>% str_remove_all("\\.$|\\/")
    ) %>%
    separate(command, into = c("col_name_value","label"), sep = "\'|\"", fill = "right") %>%
    mutate(col_name_value = str_squish(col_name_value)) %>%
    separate(col_name_value, into = c("col_name","level"), sep = " ", fill = "left") %>%
    mutate(
      col_name = if_else(is.na(col_name) & is.na(label), level, col_name),
      level = as.numeric(level)
    )  %>%
    fill(col_name, .direction = "down") %>%
    filter(!is.na(label))

  df %>%
    nest(value_labels = -col_name)
}

get_spss_missing_values <- function(df){
  # not currently implemented
  df
}
