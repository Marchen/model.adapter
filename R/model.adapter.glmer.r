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
	family = function() {
		"Get family. If family was not specified, return NULL."
		if (!is.null(call)) {
			return(info$call$family)
		} else {
			return(info$object@call$family)
		}
	}
)


	}
)


