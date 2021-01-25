get_spss_variable_labels <- function(command){

  # only takes a character vector
  stopifnot(is.character(command))

  # todo: I guess it would be better to change the following to gsubs for less dependencies?

  # collapse vector to one element
  command <- unlist(paste(command, collapse = " "))
  # remove the first two words (variable labels) and the final period
  command <- str_squish(command)
  command <- str_remove(command, "^.*? .*? ")
  command <- str_remove(command ,"\\.$")
  # separate into different vectors based on quotation marks. Need to unlist again
  command <- unlist(str_split(command, "'"))
  # get rid of any empty strings
  command <- command[command != ""]

  tibble(
    command = command,
    # we know that a name will be followed by a label
    # but, can be multiple names. So what then? I'll need to separate and fill.
    name = rep(c("col_name","col_label"), length(command)/2),
    id = (1:length(command) + 1) %/% 2
  ) %>%
    mutate(command = str_squish(command)) %>%
    pivot_wider(names_from = name, values_from = command) %>%
    select(-id)
}
