#-------------------------------------------------------------------------------
#	glmmML関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.interface class for glmmML
#'
#'	This reference class contains methods for \code{\link[glmmML]{glmmML}} in 
#'	\emph{glmmML} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.glmmML
#'	@exportClass model.interface.glmmML
#-------------------------------------------------------------------------------
model.interface.glmmML <- setRefClass(
	"model.interface.glmmML", contains = "model.interface"
)


#-------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#-------------------------------------------------------------------------------
model.interface.glmmML$methods(
	get.family = function(x) {
		if (is.call(x)) {
			return(x$family)
		} else {
			return(x$call$family)
		}
	}
)


