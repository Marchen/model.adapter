#-------------------------------------------------------------------------------
#'	model.adapter class for gamm
#'
#'	This reference class contains methods for \code{\link[mgcv]{gamm}} in 
#'	\emph{mgcv} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	gamm関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.gamm <- setRefClass(
	"model.adapter.gamm", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#-------------------------------------------------------------------------------
model.adapter.gamm$methods(
	get.family = function() {
		"Get family. If family was not specified, return NULL."
		if (!is.null(call)) {
			return(src$call$family)
		} else {
			return(src$object$gam$family)
		}
	}
)


	}
)


