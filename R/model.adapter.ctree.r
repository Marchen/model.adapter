#-------------------------------------------------------------------------------
#'	model.adapter class for ctree
#'
#'	This reference class contains methods for \code{\link[party]{ctree}} in 
#'	\emph{party} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	ctree関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.BinaryTree <- setRefClass(
	"model.adapter.BinaryTree", contains = "model.adapter"
)

model.adapter.BinaryTree$methods(
	get.call = function(x) {
	   	return(NULL)
	}
)


