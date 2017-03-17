#------------------------------------------------------------------------------
#	glmmML関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
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
#------------------------------------------------------------------------------
model.interface.glmmML <- setRefClass(
	"model.interface.glmmML", contains = "model.interface"
)


#------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#------------------------------------------------------------------------------
model.interface.glmmML$methods(
	get.family = function(x, type = c("character", "family")) {
		if (is.call(x)) {
			family <- family.from.call(x)
		} else {
			family <- x$call$family
		}
		return(format.family(family, type))
	}
)


#------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#------------------------------------------------------------------------------
model.interface.glmmML$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)


