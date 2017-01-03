#-------------------------------------------------------------------------------
#	glmer関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------
model.interface.glmerMod <- setRefClass(
	"model.interface.glmerMod", contains = "model.interface"
)


#-------------------------------------------------------------------------------
#	モデル作成に使われたデータを返す。
#-------------------------------------------------------------------------------
model.interface.glmerMod$methods(
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
model.interface.glmerMod$methods(
	get.family = function(x, type = c("character", "family")) {
		type <- match.arg(type)
		if (is.call(x)) {
			family <- x$family
		} else {
			family <- x@call$family
		}
		return(format.family(family, type))
	}
)





