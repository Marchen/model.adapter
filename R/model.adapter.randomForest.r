#-------------------------------------------------------------------------------
#'	model.adapter class for randomForest
#'
#'	This reference class contains methods for 
#'	\code{\link[randomForest]{randomForest}} in \emph{randomForest} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	randomForest関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.randomForest <- setRefClass(
	"model.adapter.randomForest", contains = "model.adapter"
)


