#-------------------------------------------------------------------------------
#	MCMCglmm関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.interface class for MCMCglmm
#'
#'	This reference class contains methods for \code{\link[MCMCglmm]{MCMCglmm}} 
#'	in \emph{MCMCglmm} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.MCMCglmm
#'	@exportClass model.interface.MCMCglmm
#-------------------------------------------------------------------------------
model.interface.MCMCglmm <- setRefClass(
	"model.interface.MCMCglmm", contains = "model.interface"
)


#-------------------------------------------------------------------------------
#	モデルのdataを取得する。
#-------------------------------------------------------------------------------
model.interface.MCMCglmm$methods(
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
model.interface.MCMCglmm$methods(
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
model.interface.MCMCglmm$methods(
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
model.interface.MCMCglmm$methods(
	get.call = function(x) {
		return(NULL)
	}
)


