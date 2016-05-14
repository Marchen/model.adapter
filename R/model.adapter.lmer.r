#-------------------------------------------------------------------------------
#	lmer関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for lmer
#'
#'	This reference class contains methods for \code{\link[lme4]{lmer}} in 
#'	\emph{lme4} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#'	@family model.adapter
#'	@export model.adapter.lmerMod
#'	@exportClass model.adapter.lmerMod
#-------------------------------------------------------------------------------
model.adapter.lmerMod <- setRefClass(
	"model.adapter.lmerMod", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	モデル作成に使われたデータを返す。
#-------------------------------------------------------------------------------
model.adapter.lmerMod$methods(
	get.data = function(x, envir = parent.frame()) {
		adapter <- model.adapter.glmerMod(x)
		return(adapter$data)
	}
)


)

