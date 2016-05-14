#-------------------------------------------------------------------------------
#	lme関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for lme
#'
#'	This reference class contains methods for \code{\link[nlme]{lme}} in 
#'	\emph{nlme} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#'	@family model.adapter
#'	@export model.adapter.lme
#'	@exportClass model.adapter.lme
#-------------------------------------------------------------------------------
model.adapter.lme <- setRefClass(
	"model.adapter.lme", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	モデルオブジェクトからcallを取得する。
#-------------------------------------------------------------------------------
model.adapter.lme$methods(
	get.call = function(x) {
		call.list <- as.list(x$call)
		call.list[[1]] <- substitute(lme)
		return(as.call(call.list))
	}
)


#-------------------------------------------------------------------------------
#	モデル構築に使われる引数からモデル式をあらわすformulaを取得する。
#-------------------------------------------------------------------------------
model.adapter.lme$methods(
	get.formula = function(x, envir = parent.frame()) {
		# Get call and convert it to a list / callを取得しリストに変換。
		if (is.object(x)) {
			call <- x$call
		} else {
			call <- x
		}
		call <- match.call(lme, call)
		args <- lapply(as.list(call), eval, envir = envir)
		return(args$fixed)
	}
)


