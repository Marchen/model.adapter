#-------------------------------------------------------------------------------
#	モデルの予測値を格納したma.predictionオブジェクトを生成する。
#	fitはランダム効果のあるモデルだったらpopulation mean。
#-------------------------------------------------------------------------------
#'	ma.prediction-class.
#'
#'	ma.prediction function initialize ma.prediction object.
#'
#'	@param fit
#'		a matrix containing predicted result. For a continuous variable,
#'		predicted values are placed in the first column. Also, if the 
#'		prediction has interval  (e.g., confidence interval, prediction 
#'		interval, etc) upper and lower values of it should be placed in 
#'		the second and third columns, respectively.
#'	@param type
#'		a character literal representing type of prediction. Currently, 
#'		"regression", "probability" and  "class" are supported.
#'	@param fixed
#'		a data.frame containing data used for prediction.
#'	@param interval.type
#'		a character literal representing type of interval.
#'	@param interval.level
#'		a numeric value representing level of interval.
#'	@return
#'		a object of ma.prediction class with following fields.
#'		\describe{
#'			\item{fit}{
#'				a matrix containing predicted result. For continuous variable, 
#'				predicted values are placed in "fit" column. If an interval 
#'				(e.g., confidence, prediction, etc.) is available, its
#'				upper and lower values are placed in "upper" and "lower"
#'				columns.
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
#-------------------------------------------------------------------------------
ma.prediction <- function(
	fit, type = c("response", "link", "probability", "class"), fixed = NULL,
	interval.type = NULL, interval.level = NULL, ...
) {
	type <- match.arg(type)
	if (!is.null(interval.type)) {
		interval.type <- match.arg(interval.type, c("confidence", "prediction"))
	}
	UseMethod("ma.prediction")
}

#-------------------------------------------------------------------------------
#'	@describeIn ma.prediction
#'	Default method handling vectors.
#'	@method ma.prediction default
#-------------------------------------------------------------------------------
ma.prediction.default <- function(
	fit, type = c("response", "link", "probability", "class"), fixed = NULL,
	interval.type = NULL, interval.level = NULL, ...
) {
	if (is.atomic(fit)) {
		fit <- as.matrix(fit, ncol = 1)
		obj <- ma.prediction(fit, type, fixed, interval.type, interval.level)
		return(obj)
	} else {
	   	stop("'fit' should be matrix, atomic or ma.prediction object.")
	}
}


#-------------------------------------------------------------------------------
#'	@describeIn ma.prediction
#'	Method for matrix.
#'	@method ma.prediction matrix
#-------------------------------------------------------------------------------
ma.prediction.matrix <- function(
	fit, type = c("response", "link", "probability", "class"), fixed = NULL,
	interval.type = NULL, interval.level = NULL, ...
) {
	# Assign column name / 列名を付ける。
	if (ncol(fit) == 1) {
		colnames(fit) <- "fit"
	} else if (ncol(fit) == 3) {
		colnames(fit) <- c("fit", "upper", "lower")
	} else {
		stop("Number of columns of 'prediction' should be one or three.")
	}
	# Assign default values.
	# デフォルト値の割り当て。
	if (!is.null(interval.type)) {
		if (interval.type == "none") {
			interval.type <- NULL
			interval.level <- NULL
		}
	}
	if (is.null(interval.level)) {
		if (!is.null(interval.type)) {
			interval.level <- 0.95
		}
	}
	# Make object / オブジェクト作成。
	obj <- list(
		fit = fit, type = type, fixed = fixed,
		interval.type = interval.type, interval.level = interval.level
	)
	class(obj) <- "ma.prediction"
	return(obj)
}


#-------------------------------------------------------------------------------
#'	@describeIn ma.prediction
#'	Method for ma.prediction.
#'	@method ma.prediction ma.prediction
#-------------------------------------------------------------------------------
ma.prediction.ma.prediction <- function(
	fit, type = c("response", "link", "probability", "class"), fixed = NULL,
	interval.type = NULL, interval.level = NULL, ...
) {
	# Update fields / フィールドの更新。
	if (!is.null(fixed)) {
		fit$fixed <- fixed
	}
	if (!is.null(interval.type)) {
		fit$interval.type <- interval.type
	}
	if (!is.null(interval.level)) {
		fit$interval.level <- interval.level
	}
	return(fit)
}




