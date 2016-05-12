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
#-------------------------------------------------------------------------------
model.adapter.lmerMod <- setRefClass(
	"model.adapter.lmerMod", contains = "model.adapter"
)

)

