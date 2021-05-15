#' Produces the string for one day of the song.
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
sing_day <- function(dataset, line, phrase_col){

  num_word <- ordinal(line)
  start <- glue("On the {num_word} day of Christmas, my true love gave to me")

  phrases <- dataset %>%
    pull({{phrase_col}})

  end <- phrases[1]

  if (line == 1) {
    return(glue("{start} {end}."))
  }

  else {
    lines <- phrases[line:2]
    lines <- paste(lines, collapse = ", \n")
    lines <- paste(lines, sep = ",\n", "and")
    return(glue("{start} \n{lines} {end}."))
  }
}
