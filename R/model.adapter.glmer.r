#'	@include model.adapter.default.r
#-------------------------------------------------------------------------------
#'	model.adapter class for glmer
#'
#'	This reference class contains methods for \code{\link[lme4]{glmer}} in 
#'	\emph{lme4} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	glmer関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.glmerMod <- setRefClass(
	"model.adapter.glmerMod", contains = "model.adapter"
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


