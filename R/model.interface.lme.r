#-------------------------------------------------------------------------------
#	lme関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.interface class for lme
#'
#'	This reference class contains methods for \code{\link[nlme]{lme}} in 
#'	\emph{nlme} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.lme
#'	@exportClass model.interface.lme
#-------------------------------------------------------------------------------
model.interface.lme <- setRefClass(
	"model.interface.lme", contains = "model.interface"
)


#-------------------------------------------------------------------------------
#	モデルオブジェクトからcallを取得する。
#-------------------------------------------------------------------------------
model.interface.lme$methods(
	get.call = function(x) {
		call.list <- as.list(x$call)
		call.list[[1]] <- substitute(lme)
		return(as.call(call.list))
	}
)


#-------------------------------------------------------------------------------
#	モデル構築に使われる引数からモデル式をあらわすformulaを取得する。
#-------------------------------------------------------------------------------
model.interface.lme$methods(
	get.formula = function(x, envir = parent.frame()) {
		# Get call and convert it to a list / callを取得しリストに変換。
		if (is.object(x)) {
			cl <- x$call
		} else {
			cl <- x
		}
		cl <- match.call(lme, cl)
		args <- lapply(as.list(cl), eval, envir = envir)
		return(args$fixed)
	}
)


#-------------------------------------------------------------------------------
#	predictメソッド。
#	random効果のmarginalizeのため、levelsを0に設定。
#-------------------------------------------------------------------------------
model.interface.lme$methods(
	predict = function(object, newdata, ...) {
		# set level = 0 to marginalize random effect.
		fit <- stats::predict(object, newdata, level = 0, ...)
		fit <- ma.prediction(fit, type = "regression")
		return(fit)
	}
)


#-------------------------------------------------------------------------------
#	モデルオブジェクトから切片の推定値を取得する。
#-------------------------------------------------------------------------------
model.interface.lme$methods(
	get.intercept = function(object) {
		return(object$coefficients$fixed["(Intercept)"])
	}
)


