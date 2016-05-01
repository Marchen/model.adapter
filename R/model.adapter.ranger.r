#-------------------------------------------------------------------------------
#'	model.adapter class for ranger
#'
#'	This reference class contains methods for \code{\link[ranger]{ranger}} in 
#'	\emph{ranger} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	ranger関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.ranger <- setRefClass(
	"model.adapter.ranger", contains = "model.adapter"
)


