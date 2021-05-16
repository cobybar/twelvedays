#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#' source("R/plotting-fun.R")
#' @export
make_phrase <- function(num, num_word, item, verb, adjective, location){

  verb <- str_replace_na(verb, "")
  adjective <- str_replace_na(adjective, "")
  location <- str_replace_na(location, "")
  num_word <- english(num, english.UK = FALSE)

  if (num == 1) {
    num_word <- "a"
  }
  else {
    item <- pluralize_gift(item)
  }

  phrase <- glue("{num_word} {adjective} {item} {verb} {location}")

  return(str_squish(phrase))
}

