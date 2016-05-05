#-------------------------------------------------------------------------------
#'	model.adapter class for lme
#'
#'	This reference class contains methods for \code{\link[nlme]{lme}} in 
#'	\emph{nlme} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	lme関数用のmodel.adapterオブジェクトのジェネレーター。
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
	get.formula = function(x) {
		# Get call and convert it to a list / callを取得しリストに変換。
		if (is.object(x)) {
			call <- x$call
		} else {
			call <- x
		}
		call <- match.call(lme, call)
		args <- lapply(as.list(call), eval)
		return(args$fixed)
	}
)


