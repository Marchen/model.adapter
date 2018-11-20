#------------------------------------------------------------------------------
#'	(Internal) model.interface class for ranger
#'
#'	This reference class contains methods for \code{\link[ranger]{ranger}} in
#'	\emph{ranger} package.
#'
#'	@include model.interface.default.r
#'	@name model.interface.ranger-class (ranger)
#------------------------------------------------------------------------------
model.interface.ranger.class <- R6::R6Class(
	"model.interface.ranger", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#'	@method model.interface ranger
#'	@export
#'	@describeIn model.interface S3 method for class 'ranger'
#------------------------------------------------------------------------------
model.interface.ranger <- model.interface.ranger.class$new


#------------------------------------------------------------------------------
model.interface.ranger.class$set(
	"public", "predict",
	function(object, newdata = NULL, type, ...) {
		# If no newdata specified, prepare newdata using get.data() method.
		if (is.null(newdata)) {
			package <- package.name(object, parent.frame())
			newdata <- self$get.data(object, parent.frame(), package)
		}
		# name of 'newdata' argument is 'data' for ranger.
		if (type == "prob") {
			# If type is "prob", calculate probability.
			pred <- stats::predict(
				object, data = newdata, predict.all = TRUE, ...
			)
			n.votes <- apply(
				pred$predictions, 1, function(x) tapply(x, x, length)
			)
			n.votes <- lapply(
				n.votes, "[", as.character(unique(c(pred$predictions)))
			)
			prob <- do.call(rbind, n.votes) / pred$num.trees
			colnames(prob) <- levels(object$predictions)
			prob[is.na(prob)] <- 0
			return(prob)
		} else {
			pred <- stats::predict(object, data = newdata, ...)
			return(pred$predictions)
		}
	}
)


#------------------------------------------------------------------------------
model.interface.ranger.class$set(
	"public", "get.formula",
	function(x, envir, package = "") {
		f <- super$get.formula(x, envir, package)
		if (is.null(f)) {
			call <- match.generic.call(x$call, envir, package)
			f <- formula(call$formula)
		}
		return(f)
	}
)
