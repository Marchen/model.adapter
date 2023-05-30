#------------------------------------------------------------------------------
#' (Internal) Prepare predict.types.
#'
#' Prepare a named character vector for predict.types.
#'
#' @param predict.types a character vector of predict.types.
#' @param response a character representing predict type of reponse scale.
#' @param link a character representing predict type of link scale.
#' @param prob a character representing predict type of class probability.
#' @param class a character representing predict type of class labels.
#'
#' @return a character representing predict.types.
#------------------------------------------------------------------------------
make.predict.types <- function(
    predict.types,
    response = "response", link = "link", prob = "prob", class = "class"
) {
    if (missing(predict.types)) {
        predict.types <- c(response, link, prob, class)
    }
    names(predict.types) <- c("response", "link", "prob", "class")
    return(predict.types)
}
