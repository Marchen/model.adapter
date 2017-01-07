#-------------------------------------------------------------------------------
#	lmer関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.interface class for lmer
#'
#'	This reference class contains methods for \code{\link[lme4]{lmer}} in
#'	\emph{lme4} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.lmerMod
#'	@exportClass model.interface.lmerMod
#-------------------------------------------------------------------------------
model.interface.lmerMod <- setRefClass(
	"model.interface.lmerMod", contains = "model.interface"
)


#-------------------------------------------------------------------------------
#	モデル作成に使われたデータを返す。
#-------------------------------------------------------------------------------
model.interface.lmerMod$methods(
	get.data = function(x, envir = parent.frame()) {
		interface <- model.interface.glmerMod(x)
		return(interface$get.data(x, envir))
	}
)


#-------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#-------------------------------------------------------------------------------
model.interface.lmerMod$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)


