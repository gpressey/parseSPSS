#' Get SPSS Text File Delimitors
#'
#' @param df A data file consisting of a complete 'data file list' command to read a fixed width text file
#'
#' @return A fwf_positions object
#' @export
#'
#' @examples
get_spss_delims <- function(command){

  # only takes a character vector
  stopifnot(is.character(command))

  # todo: I guess it would be better to change the following to gsubs for less dependencies?

  # collapse vector to one element
  command <- unlist(paste(command, collapse = " "))
  command <- str_squish(command)
  # remove the first two words (variable labels) and the final period
  command <- str_remove(command, "^.*'")
  command <- str_remove(command ,"\\.$")
  command <- str_remove_all(command, "/") # also any endline backslashes
  command <- str_remove_all(command, "-") # any dashes between positions
  # separate into different vectors based on quotation marks. Need to unlist again
  command <- unlist(str_split(command, " (?=[:alpha:])"))
  # get rid of any empty strings
  command <- command[command != ""]

  df <- tibble(
    command = str_squish(command)
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
