#-------------------------------------------------------------------------------
#	tree関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for tree
#'
#'	This reference class contains methods for \code{\link[tree]{tree}} in 
#'	\emph{tree} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#'	@family model.adapter
#'	@export model.adapter.tree
#'	@exportClass model.adapter.tree
#-------------------------------------------------------------------------------
model.adapter.tree <- setRefClass(
	"model.adapter.tree", contains = "model.adapter"
)


