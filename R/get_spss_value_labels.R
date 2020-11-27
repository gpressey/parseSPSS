get_spss_value_labels <- function(command){
  # only takes a character vector
  stopifnot(is.character(command))

  # collapse vector to one element
  command <- unlist(paste(command, collapse = " "))
  # remove the first two words (variable labels) and the final period
  command <- str_remove(command, "^.*? .*? ")
  command <- str_remove(command ,"\\.$")
  command <- str_remove_all(command, "/") # also any endline backslashes

  # separate into different vectors based on quotation marks. Need to unlist again
  command <- unlist(str_split(command, "'"))
  # get rid of any empty strings
  command <- unlist(str_squish(command))
  command <- command[command != ""]

  tibble(
    command = command,
    type = rep(c("col_name_value","label"), length(command)/2)
  ) %>%
    mutate(
      # need to change it to before the first numeric value --
      # currently it won't handle several variables with the same
      # labels very well
      col_name = if_else(type == "col_name_value" & str_detect(command, "^[:alpha:]"), str_remove(command, " .*$"), NA_character_),
      command =  if_else(type == "col_name_value" & str_detect(command, "^[:alpha:]"), str_remove(command, "^.* "), command)
    ) %>%
    fill(col_name, .direction = "down") %>%
    group_by(col_name) %>%
    mutate(id = (1:n() + 1) %/% 2) %>%
    pivot_wider(names_from = type, values_from = command) %>%
    mutate(col_name_value = as.numeric(col_name_value)) %>%
    select(-id)

}
