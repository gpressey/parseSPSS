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
