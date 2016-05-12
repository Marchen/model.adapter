#'	@include model.adapter.default.r
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


#-------------------------------------------------------------------------------
#	formulaを取り出し。
#-------------------------------------------------------------------------------
model.adapter.BinaryTree$methods(
	get.formula = function(x, envir = parent.frame()) {
		# Shared method with cforest / cforestと同じ手法。
		adapter <- model.adapter.RandomForest(x, envir)
		return(adapter$get.formula(x, envir))
	}
)


