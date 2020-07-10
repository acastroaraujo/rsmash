
##### SOMETHING IS WRONG HERE, THE EXPANDED DATA FRAME HAS WAY LESS ROWS THAN IT SHOULD

#' Lasso Model
#'
#' @param token_data_frame tokens
#' @param original_data_frame origianl data frame
#' @param lambda the type of lambda to choose from cross-validation (default: min)
#'
#' @return A list with three elements: model coefficients, test accuracy, expanded data frame
#' @export
#'
smash_lasso <- function(token_data_frame, original_data_frame, lambda = c("min", "1se")) {

  lambda_type <- paste0("lambda.", match.arg(lambda))

  design_matrix <- token_data_frame %>%
    dplyr::count(.data$review_n, .data$word) %>%
    dplyr::arrange(.data$review_n) %>%
    tidytext::cast_sparse(.data$review_n, .data$word, .data$n)

  outcome <- dplyr::distinct(token_data_frame, .data$review_n, .data$outcome)$outcome == "Positive"

  ## Model ----

  lasso_model <- glmnet::cv.glmnet(
    x = design_matrix,
    y = outcome,
    family = "binomial",
    alpha = 1,
    nfold = 10
  )

  model_coefficients <- lasso_model$glmnet.fit %>%
    broom::tidy() %>%
    dplyr::filter(lambda == lasso_model[[lambda_type]])

  original_data_frame_expanded <- token_data_frame %>%
    dplyr::inner_join(model_coefficients, by = c("word" = "term")) %>%
    dplyr::mutate(estimate = ifelse(is.na(.data$estimate), 0, .data$estimate)) %>%
    dplyr::group_by(.data$review_n) %>%
    dplyr::summarize(linpred_sans_intercept = sum(.data$estimate)) %>%
    dplyr::mutate(probability = plogis((model_coefficients %>% dplyr::filter(term == "(Intercept)") %>% dplyr::pull(.data$estimate)) + .data$linpred_sans_intercept)) %>%          ## replace with intercept
    dplyr::mutate(predictions = ifelse(probability > 0.5, "Good", "Bad")) %>% ## Change the threshold if necessary
    dplyr::inner_join(original_data_frame) %>%
    dplyr::select(.data$review_n, .data$rating, .data$predictions, .data$probability, .data$raw_text, .data$text)

  output <- list(
    coefficients = model_coefficients,
    expanded_data_frame = original_data_frame_expanded
  )

  return(output)

}

smash_lasso_test_accuracy <- function(test_data_frame, smash_lasso_obj) {


}

