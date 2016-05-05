#-------------------------------------------------------------------------------
#'	model.adapter class for ctree
#'
#'	This reference class contains methods for \code{\link[party]{ctree}} in 
#'	\emph{party} package.
#'	Note that because an object of BinaryTree does not keep original call,
#'	get.call() function always returns NULL. Also, when an instance of this 
#'	class is made from model object, 'call' field is always call("<undef>").
#'
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	ctree関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.BinaryTree <- setRefClass(
	"model.adapter.BinaryTree", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	モデルオブジェクトからcallを取得する。
#-------------------------------------------------------------------------------
model.adapter.BinaryTree$methods(
	get.call = function(x) {
		return(NULL)
	}
)


