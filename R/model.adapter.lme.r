#-------------------------------------------------------------------------------
#'	model.adapter class for lme
#'
#'	This reference class contains methods for \code{\link[nlme]{lme}} in 
#'	\emph{nlme} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	lme関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.lme <- setRefClass(
	"model.adapter.lme", contains = "model.adapter"
)
