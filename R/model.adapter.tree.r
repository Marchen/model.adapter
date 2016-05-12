#'	@include model.adapter.default.r
#-------------------------------------------------------------------------------
#'	model.adapter class for tree
#'
#'	This reference class contains methods for \code{\link[tree]{tree}} in 
#'	\emph{tree} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	tree関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.tree <- setRefClass(
	"model.adapter.tree", contains = "model.adapter"
)


