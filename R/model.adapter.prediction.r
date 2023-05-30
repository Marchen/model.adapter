#------------------------------------------------------------------------------
#' model.adapter.prediction-class
#'
#' @field fit
#'      a matrix containing predicted result. For continuous variables,
#'      predicted values are placed in "fit" column. If an interval
#'      (e.g., confidence, prediction, etc.) is available, iupper and lower
#'      values are placed in "upper" and "lower" columns.
#'
#'      For discrete variables, content of this field depends on value of
#'      \emph{type} field. If \emph{type} is "probability", fit is a matrix
#'      with columns representing probability of each class.
#'      If response variable is binary (0/1) variable, fit is a matrix with the
#'      first column representing probability of "0" and the second column
#'      representing probability of "1".
#'
#'      For classification models and type = "class", predicted classes.
#'
#' @field type
#'      a character literal representing type of prediction, one of
#'      "response", "link", "prob" and  "class".
#'
#' @field fixed
#'      a data.frame containing data used for prediction.
#'      If original data.frame is not available, this field is NULL.
#'
#' @field interval.type
#'      a character literal representing type and level of the interval.
#'      If no interval is available, this field is NULL.
#'      If the interval is confidence interval, this field has "confidence".
#'      If the interval is prediction interval, this field has "prediction".
#'
#' @field interval.level
#'      a number representing level of interval (0 <= interval.level <= 1).
#'      If no interval is calculated, this field should be NULL.
#'
#' @name model.adapter.prediction-class
#------------------------------------------------------------------------------
model.adapter.prediction.class <- R6::R6Class(
    "model.adapter.prediction",
    public = list(
        fit = NA,
        type = NA,
        fixed = NA,
        interval.type = NA,
        interval.level = NA
    )
)


#------------------------------------------------------------------------------
#   Initialize model.adapter.prediction object
#------------------------------------------------------------------------------
model.adapter.prediction.class$set(
    "public", "initialize",
    function(
        fit, type = c("response", "link", "prob", "class"), fixed = NULL,
        interval.type = NULL, interval.level = NULL, logical.response = FALSE
    ) {
        # Convert 'fit' to matrix.
        if (!is.matrix(fit)) {
            if (is.atomic(fit)) {
                f <- as.matrix(fit, ncol = 1)
            } else {
                stop("'fit' should be matrix or atomic.")
            }
        } else {
            f <- fit
        }
        # Check error in type.
        self$type <- match.arg(type)
        # Initialize fields.
        private$init.fit(f, type, logical.response)
        private$init.interval(interval.type, interval.level)
    }
)


#------------------------------------------------------------------------------
#   Initialize fit field of a model.adapter.prediction object
#------------------------------------------------------------------------------
model.adapter.prediction.class$set(
    "private", "init.fit",
    function(fit, type, logical.response) {
        self$fit <- fit
        # Assign column name.
        if (type %in% c("response", "link")) {
            if (ncol(self$fit) == 1) {
                colnames(self$fit) <- "fit"
            } else if (ncol(self$fit) == 3) {
                if (self$fit[1, 3] > self$fit[1, 2]) {
                    self$fit <- self$fit[, c(1, 3, 2)]
                }
                colnames(self$fit) <- c("fit", "upper", "lower")
            } else {
                stop(
                    "Number of columns of 'prediction' should be one or three."
                )
            }
        }
        # Convert result of binary response model (e.g. logistic regression)
        # to probability of 0 and 1 or FALSE and TRUE.
        if (type == "prob" & ncol(self$fit) == 1) {
            self$fit <- cbind(1 - self$fit, self$fit)
            fun <- ifelse(logical.response, as.logical, identity)
            colnames(self$fit) <- fun(0:1)
        }
        # Convert result of binary response model to "0" and "1" with the
        # threshold of 0.5.
        if (type == "class" & ncol(self$fit) == 1 & is.numeric(self$fit)) {
            self$fit <- ifelse(self$fit >= 0.5, "1", "0")
        }
    }
)


#------------------------------------------------------------------------------
#   Initialize intervals
#------------------------------------------------------------------------------
model.adapter.prediction.class$set(
    "private", "init.interval",
    function(
        interval.type = c("none", "confidence", "prediction"), interval.level
    ) {
        # Check error in interval.type.
        if (!is.null(interval.type)) {
            self$interval.type <- match.arg(interval.type)
        } else {
            self$interval.type <- interval.type
        }
        self$interval.level <- interval.level
        if (!is.null(self$interval.type)) {
            if (self$interval.type == "none") {
                self$interval.type <- NULL
                self$interval.level <- NULL
            }
        }
        if (is.null(self$interval.level)) {
            if (!is.null(self$interval.type)) {
                self$interval.level <- 0.95
            }
        }
    }
)


#------------------------------------------------------------------------------
#' (Internal) Initialize model.adapter.prediction object
#'
#' model.adapter.prediction function initialize model.adapter.prediction
#' object.
#'
#' @param fit
#'      a matrix containing predicted result.
#'      For continuous response variables, predicted values should be placed in
#'      the first column. Also, if the prediction has interval (e.g., confidence
#'      interval, prediction interval, etc.) upper and lower values of them
#'      should be placed in the second and third columns, respectively.
#'
#'      For binary model, matrix with one column with probability of positive
#'      case or two columns with probability of negative and positive cases.
#'      For classification model with >3 classes, probability of each class.
#'
#'      For classification models and type = "class", predicted classes.
#'
#' @param type
#'      a character literal representing type of prediction. Currently,
#'      "response", "link", "prob" and  "class" are supported.
#'
#' @param fixed
#'      a data.frame containing data used for prediction.
#'
#' @param interval.type
#'      a character literal representing type of interval.
#'
#' @param interval.level
#'      a numeric value representing level of interval.
#'
#' @param logical.response
#'      a logical indicationg response variable is logical.
#'
#' @return
#'      an object of
#'      \code{\link[=model.adapter.prediction.class]{model.adapter.prediction}}
#'      class.
#------------------------------------------------------------------------------
model.adapter.prediction <- function(
    fit, type = c("response", "link", "prob", "class"), fixed = NULL,
    interval.type = NULL, interval.level = NULL, logical.response = FALSE
) {
    object <- model.adapter.prediction.class$new(
        fit = fit, type = type, fixed = fixed, interval.type = interval.type,
        interval.level = interval.level, logical.response = logical.response
    )
    return(object)
}
