#-------------------------------------------------------------------------------
#	glmmML関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for glmmML
#'
#'	This reference class contains methods for \code{\link[glmmML]{glmmML}} in 
#'	\emph{glmmML} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#'	@family model.adapter
#'	@export model.adapter.glmmML
#'	@exportClass model.adapter.glmmML
#-------------------------------------------------------------------------------
model.adapter.glmmML <- setRefClass(
	"model.adapter.glmmML", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#-------------------------------------------------------------------------------
model.adapter.glmmML$methods(
	get.family = function(x) {
		if (is.call(x)) {
			return(x$family)
		} else {
			return(x$call$family)
		}
	}
)


