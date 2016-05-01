#-------------------------------------------------------------------------------
#'	model.adapter class for lm
#'
#'	This reference class contains methods for \code{\link[stats]{lm}} in 
#'	\emph{stats} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	lm関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.lm <- setRefClass(
	"model.adapter.lm", contains = "model.adapter"
)



