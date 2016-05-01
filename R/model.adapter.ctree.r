#-------------------------------------------------------------------------------
#'	model.adapter class for ctree
#'
#'	This reference class contains methods for \code{\link[party]{ctree}} in 
#'	\emph{party} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	ctree関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.ctree <- setRefClass(
	"model.adapter.ctree", contains = "model.adapter"
)


