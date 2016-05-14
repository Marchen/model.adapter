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
#'	@family model.adapter
#'	@export model.adapter.MCMCglmm
#'	@exportClass model.adapter.MCMCglmm
#-------------------------------------------------------------------------------
model.adapter.MCMCglmm <- setRefClass(
	"model.adapter.MCMCglmm", contains = "model.adapter"
)

#-------------------------------------------------------------------------------
#	Initialize call field using match.generic.call().
#	match.generic.call()でcallを初期化。
#-------------------------------------------------------------------------------
model.adapter.MCMCglmm$methods(
	initialize = function(x, envir = parent.frame(4L), data = NULL, ...) {
		x <- substitute(x)
		callSuper(x, envir, caller = "subclass")
		if (!is.null(data)){
			data <<- data
		}
	}
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


