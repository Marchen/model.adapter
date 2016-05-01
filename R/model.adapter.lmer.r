#-------------------------------------------------------------------------------
#'	model.adapter class for lmer
#'
#'	This reference class contains methods for \code{\link[lme4]{lmer}} in 
#'	\emph{lme4} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	lmer関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.lmerMod <- setRefClass(
	"model.adapter.lmerMod", contains = "model.adapter"
)

)

