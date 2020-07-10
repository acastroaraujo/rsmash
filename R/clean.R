

#' Clean raw text
#'
#' @param data_frame a data frame with a `raw_text` and a `rating` column
#'
#' @return a data frame with a new column called `text`
#' @export
#'
smash_clean <- function(data_frame, parallel = TRUE){

  if (!all(c("raw_text", "rating") %in% colnames(data_frame))) stop(call. = FALSE, "Your data frame must have at least columns <<raw_text>> and <<rating>>")

  if (parallel) {

    message("Using ", parallel::detectCores() - 1, " cores for data cleaning")
    message("This won't work on Windows")
    text_processed <- parallel::mclapply(data_frame$raw_text, clean_text, mc.cores = parallel::detectCores() - 1)

  } else {

    text_processed <- purrr::map(data_frame$raw_text, clean_text)

  }

  data_frame %>%
    dplyr::mutate(text = as.character(text_processed),
                  review_n = dplyr::row_number(),
                  outcome = dplyr::case_when(
                    .data$rating < 3 ~ "Negative",
                    .data$rating > 3 ~ "Positive")) %>%
    dplyr::filter(!is.na(.data$outcome))

}


clean_text <- function(x) {

  x %>%
    textclean::replace_url() %>%
    textclean::replace_internet_slang() %>%
    textclean::replace_word_elongation() %>%
    textclean::replace_contraction() %>%
    textclean::replace_money() %>%
    textclean::replace_names() %>%
    textclean::replace_ordinal() %>%
    textclean::replace_emoji() %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()

}

#' Balance positive and negative reviews
#'
#' @param data_frame a data frame, as produced by smash_clean()
#'
#' @return a balanced data frame (i.e. equal number of positive and negative reviews)
#' @export
#'
smash_balance <- function(data_frame) {

  min_n <- data_frame %>%
    dplyr::count(.data$outcome) %>%
    dplyr::pull(.data$n) %>%
    min()

  data_frame %>%
    dplyr::group_by(.data$outcome) %>%
    dplyr::sample_n(min_n) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$review_n)
}



#' Tokenize data frame
#'
#' @param data_frame a data frame like the one produced by the smash_clean() function
#' @param min_n a number, minimum threshold for words
#' @param custom_stopwords a character vector of stopwords (optional)
#' @param ... extra arguments to internal function break_into_sentences(), (e.g. `remove_sentences = c("show more", "continue reading", ".")`, `parallel = FALSE`)
#'
#' @return a data frame of tokens
#' @export
#'
smash_tokenize <- function(data_frame, min_n = 30, custom_stopwords = NULL, ...) {

  message("Removing words used less than <<", min_n, ">> times")

  sentences <- break_into_sentences(data_frame, ...) %>%
    dplyr::mutate(text = extra_cleaning_function(.data$text, custom_stopwords))

  create_token_df(sentences, data_frame, min_n)


}


break_into_sentences <- function(data_frame, parallel = TRUE, remove_sentences = c("show more", "continue reading", ".")) {

  split_reviews_by_sentence <- function(x) {

    sentimentr::get_sentences(data_frame$text[x]) %>%
      unlist() %>%
      tibble::enframe(name = NULL, value = "text") %>%
      dplyr::mutate(review_n = x, sentence_n = dplyr::row_number())

  }

  if (parallel) {

    message("Using ", parallel::detectCores() - 1, " cores for data cleaning. This won't work on Windows")

    output <- parallel::mclapply(1:nrow(data_frame), split_reviews_by_sentence, mc.cores = parallel::detectCores() - 1) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(!.data$text %in% remove_sentences)

  } else {

    output <- purrr::map(1:nrow(data_frame), split_reviews_by_sentence) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(!.data$text %in% remove_sentences)
  }

  return(output)
}


extra_cleaning_function <- function(x, custom_stopwords){

  if (is.null(custom_stopwords)) {
    stopwords <- tidytext::stop_words %>% dplyr::pull(.data$word)
  } else{
    stopwords <- custom_stopwords
  }

  x %>%
    tm::removeNumbers() %>%
    tm::removeWords(words = stopwords) %>%
    tm::removePunctuation() %>%
    tm::stripWhitespace()

}


create_token_df <- function(sentences, original_df, min_n) {

  output <- dplyr::bind_rows(
    tidytext::unnest_tokens(sentences, word, .data$text, token = "words"),
    tidytext::unnest_tokens(sentences, word, .data$text, token = "ngrams", n = 2)
  ) %>%
    dplyr::arrange(.data$review_n, .data$sentence_n) %>%
    dplyr::select(-.data$sentence_n) %>%
    dplyr::inner_join(original_df %>% dplyr::select(.data$review_n, .data$rating, .data$outcome)) %>%
    dplyr::distinct() %>%
    tidyr::drop_na() %>%
    dplyr::group_by(word) %>%
    dplyr::filter(dplyr::n() >= min_n) %>% ## remove uncommon unigrams and bigrams MAKE THIS TUNABLE
    dplyr::ungroup()

  return(output)
}


