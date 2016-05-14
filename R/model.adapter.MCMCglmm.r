#-------------------------------------------------------------------------------
#	glm関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for MCMCglmm
#'
#'	This reference class contains methods for \code{\link[MCMCglmm]{MCMCglmm}} 
#'	in \emph{MCMCglmm} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#-------------------------------------------------------------------------------
model.adapter.MCMCglmm <- setRefClass(
	"model.adapter.MCMCglmm", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	モデルのdataを取得する。
#-------------------------------------------------------------------------------
model.adapter.MCMCglmm$methods(
	get.data = function(x, envir = parent.frame()) {
		if (is.call(x)) {
			callSuper(x, envir)
		} else {
			return(data.frame())
		}
	}
)


#-------------------------------------------------------------------------------
#	formulaを取り出し。
#-------------------------------------------------------------------------------
model.adapter.MCMCglmm$methods(
	get.formula = function(x, envir = parent.frame()) {
		if (is.call(x)) {
			return(callSuper(x, envir))
		} else {
		
		}
		if (is.object(x)) {
			return(x$Fixed$formula)
		}
	}
)


#-------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#-------------------------------------------------------------------------------
model.adapter.MCMCglmm$methods(
	get.family = function(x) {
		if (is.call(x)) {
			return(callSuper(x))
		} else {
			return(x$family[[1]])
		}
	}
)


#-------------------------------------------------------------------------------
#	モデルオブジェクトからcallを取得する。
#-------------------------------------------------------------------------------
model.adapter.MCMCglmm$methods(
	get.call = function(x) {
		return(NULL)
	}
)


