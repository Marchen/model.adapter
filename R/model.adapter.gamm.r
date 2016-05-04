#-------------------------------------------------------------------------------
#'	model.adapter class for gamm
#'
#'	This reference class contains methods for \code{\link[mgcv]{gamm}} in 
#'	\emph{mgcv} package.
#'	Note that because an object of gamm does not keep original call,
#'	get.call() function always returns NULL. Also, when an instance of this 
#'	class is made from model object, 'call' field is always call("<undef>").
#'
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	gamm関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.gamm <- setRefClass(
	"model.adapter.gamm", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	モデルオブジェクトからcallを取得する。
#-------------------------------------------------------------------------------
model.adapter.gamm$methods(
	get.call = function(x) {
	   	return(NULL)
	}
)


#-------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#-------------------------------------------------------------------------------
model.adapter.gamm$methods(
	get.family = function(x) {
		if (is.call(x)) {
		   	return(x$family)
		} else {
			return(x$gam$family)
		}
	}
)


	}
)


