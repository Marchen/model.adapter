#------------------------------------------------------------------------------
#	glmer関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
#'	model.interface class for glmer
#'
#'	This reference class contains methods for \code{\link[lme4]{glmer}} in
#'	\emph{lme4} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.glmerMod
#'	@exportClass model.interface.glmerMod
#------------------------------------------------------------------------------
model.interface.glmerMod <- setRefClass(
	"model.interface.glmerMod", contains = "model.interface"
)


#------------------------------------------------------------------------------
#	モデル作成に使われたデータを返す。
#------------------------------------------------------------------------------
model.interface.glmerMod$methods(
	get.data = function(x, envir = parent.frame(), package = "", ...) {
		if (is.call(x)){
			return(callSuper(x, envir, package, ...))
		} else {
			d <- x@frame
			attr(d, "terms") <- NULL
			return(d)
		}
	}
)


#------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#------------------------------------------------------------------------------
model.interface.glmerMod$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)





