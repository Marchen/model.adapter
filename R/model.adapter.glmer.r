#-------------------------------------------------------------------------------
#	glmer関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for glmer
#'
#'	This reference class contains methods for \code{\link[lme4]{glmer}} in 
#'	\emph{lme4} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#'	@family model.adapter
#'	@export model.adapter.glmerMod
#'	@exportClass model.adapter.glmerMod
#-------------------------------------------------------------------------------
model.adapter.glmerMod <- setRefClass(
	"model.adapter.glmerMod", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	モデル作成に使われたデータを返す。
#-------------------------------------------------------------------------------
model.adapter.glmerMod$methods(
	get.data = function(x, envir = parent.frame()) {
		if (is.call(x)){
			return(callSuper(x, envir))
		} else {
			d <- x@frame
			attr(d, "terms") <- NULL
			return(d)
		}
	}
)


#-------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#-------------------------------------------------------------------------------
model.adapter.glmerMod$methods(
	get.family = function(x) {
		if (is.call(x)) {
			return(x$family)
		} else {
			return(x@call$family)
		}
	}
)


	}
)


