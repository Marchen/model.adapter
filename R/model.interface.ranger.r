#------------------------------------------------------------------------------
#	ranger関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
#'	model.interface class for ranger
#'
#'	This reference class contains methods for \code{\link[ranger]{ranger}} in
#'	\emph{ranger} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.ranger
#'	@exportClass model.interface.ranger
#------------------------------------------------------------------------------
model.interface.ranger <- setRefClass(
	"model.interface.ranger", contains = "model.interface"
)


#------------------------------------------------------------------------------
#	ranger用predict()メソッド。
#------------------------------------------------------------------------------
model.interface.ranger$methods(
	predict = function(object, newdata = NULL, type, ...) {
		# If no newdata specified, prepare newdata using get.data() method.
		if (is.null(newdata)) {
			package <- package.name(object, parent.frame())
			newdata <- .self$get.data(object, parent.frame(), package)
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
#	formulaを取得する。
#------------------------------------------------------------------------------
model.interface.ranger$methods(
	get.formula = function(x, envir) {
		f <- callSuper(x, envir)
		if (is.null(f)) {
			call <- match.generic.call(x$call, envir)
			f <- formula(call$formula)
		}
		return(f)
	}
)

