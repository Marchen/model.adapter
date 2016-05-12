#-------------------------------------------------------------------------------
#	randomForest関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for randomForest
#'
#'	This reference class contains methods for 
#'	\code{\link[randomForest]{randomForest}} in \emph{randomForest} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#-------------------------------------------------------------------------------
model.adapter.randomForest <- setRefClass(
	"model.adapter.randomForest", contains = "model.adapter"
)


