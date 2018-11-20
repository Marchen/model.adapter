#------------------------------------------------------------------------------
#'	ma.prediction-class.
#'
#'	ma.prediction function initialize ma.prediction object.
#'
#'	@param fit
#'		a matrix containing predicted result.
#'		For continuous response variables, predicted values are placed in the
#'		first column. Also, if the prediction has interval (e.g., confidence
#'		interval, prediction interval, etc.) upper and lower values of them
#'		should be placed in the second and third columns, respectively.
#'
#'		For discrete variables, each column represents
#'
#'	@param type
#'		a character literal representing type of prediction. Currently,
#'		"response", "link", "prob" and  "class" are supported.
#'
#'	@param fixed
#'		a data.frame containing data used for prediction.
#'
#'	@param interval.type
#'		a character literal representing type of interval.
#'
#'	@param interval.level
#'		a numeric value representing level of interval.
#'
#'	@return
#'		an object of ma.prediction class with following fields.
#'		\describe{
#'			\item{fit}{
#'				a matrix containing predicted result. For continuous variables,
#'				predicted values are placed in "fit" column. If an interval
#'				(e.g., confidence, prediction, etc.) is available, its
#'				upper and lower values are placed in "upper" and "lower"
#'				columns.
#'
#'				For discrete variables, content of this field depends on
#'				\emph{type} argument. If \emph{type} is "probability", fit is a
#'				matrix with columns representing probability of each class.
#'				If response variable is binary (0/1) variable, fit is a matrix
#'				with the first column representing probability of "0" and the
#'				second column representing probability of "1".
#'
#'				If \emph{type} is "class",
#'			}
#'			\item{type}{
#'				a character literal representing type of prediction.
#'				Currently, "response", "link", "probability" and  "class" are
#'				supported.
#'			}
#'			\item{fixed}{
#'				a data.frame containing data used for prediction.
#'				If not available, this field is NULL.
#'			}
#'			\item{interval.type}{
#'				a character literal representing type and level of the
#'				interval. If no interval is available, this field is NULL.
#'				If the interval is confidence interval, this field is
#'				"confidence". If the interval is prediction interval,
#'				this field is "prediction".
#'			}
#'			\item{interval.level}{
#'				a number representing level of interval. If no interval is
#'				calculated, this field should be NULL. If
#'				\code{interval.level} is NULL and
#'			}
#'		}
#'	@export
#'
#------------------------------------------------------------------------------
ma.prediction <- setRefClass(
	"ma.prediction",
	list(
		fit = "ANY",
		type = "character",
		fixed = "ANY",
		interval.type = "ANY",
		interval.level = "ANY"
	)
)


ma.prediction$methods(
	initialize = function(
		fit, type = c("response", "link", "prob", "class"), fixed = NULL,
		interval.type = NULL, interval.level = NULL, logical.response = FALSE,
		...
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
		.self$type <- match.arg(type)
		# Initialize fields.
		.self$init.fit(f, type, logical.response)
		.self$init.interval(interval.type, interval.level)
	}
)

ma.prediction$methods(
	init.fit = function(fit, type, logical.response) {
		.self$fit <- fit
		# Assign column name.
		if (type %in% c("response", "link")) {
			if (ncol(.self$fit) == 1) {
				colnames(.self$fit) <- "fit"
			} else if (ncol(.self$fit) == 3) {
				colnames(.self$fit) <- c("fit", "upper", "lower")
			} else {
				stop(
					"Number of columns of 'prediction' should be one or three."
				)
			}
		}
		# Convert result of binary response model (e.g. logistic regression)
		# to probability of 0 and 1 or FALSE and TRUE.
		if (type == "prob" & ncol(.self$fit) == 1) {
			.self$fit <- cbind(1 - .self$fit, .self$fit)
			fun <- ifelse(logical.response, as.logical, identity)
			colnames(.self$fit) <- fun(0:1)
		}
		# Convert result of binary response model to "0" and "1" with the
		# threshold of 0.5.
		if (type == "class" & ncol(.self$fit) == 1 & is.numeric(.self$fit)) {
			.self$fit <- ifelse(.self$fit >= 0.5, "1", "0")
		}
	}
)


ma.prediction$methods(
	init.interval = function(interval.type, interval.level) {
		# Check error in interval.type.
		if (!is.null(interval.type)) {
			.self$interval.type <- match.arg(
				interval.type, c("confidence", "prediction")
			)
		} else {
			.self$interval.type <- interval.type
		}
		.self$interval.level <- interval.level
		if (!is.null(.self$interval.type)) {
			if (.self$interval.type == "none") {
				.self$interval.type <- NULL
				.self$interval.level <- NULL
			}
		}
		if (is.null(.self$interval.level)) {
			if (!is.null(.self$interval.type)) {
				.self$interval.level <- 0.95
			}
		}
	}
)
