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
sing_line <- function(dataset, line, phrase_col){

  num_word <- ordinal(line)
  song <- glue("On the {num_word} day of Christmas, my true love gave to me")

  phrases <- dataset %>%
    pull({{phrase_col}})

  phrases[1] <- paste("and ", phrases[1], sep = "", ".")
  gifts <- str_c(phrases[line:1], collapse = ", \n")

  glue("{song} \n{gifts}")

}
