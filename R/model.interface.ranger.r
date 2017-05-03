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
		# If no newdata specified, use predictions() to
		# extract predicted values.
		if (is.null(newdata)) {
			return(predictions(object))
		}
		# name of 'newdata' argument is 'data' for ranger.
		pred <- stats::predict(object, data = newdata, ...)
		return(pred$predictions)
	}
)


#------------------------------------------------------------------------------
#	formulaを取得する。
#------------------------------------------------------------------------------
model.interface.ranger$methods(
	get.formula = function(x, envir = parent.frame()) {
		f <- callSuper(x, envir)
		if (is.null(f)) {
			call <- match.generic.call(x$call)
			f <- formula(call$formula)
		}
		return(f)
	}
)

