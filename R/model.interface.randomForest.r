#------------------------------------------------------------------------------
#	randomForest関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
#'	model.interface class for randomForest
#'
#'	This reference class contains methods for
#'	\code{\link[randomForest]{randomForest}} in \emph{randomForest} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.randomForest
#'	@exportClass model.interface.randomForest
#------------------------------------------------------------------------------
model.interface.randomForest <- setRefClass(
	"model.interface.randomForest", contains = "model.interface"
)


#------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#------------------------------------------------------------------------------
model.interface.randomForest$methods(
	predict.types = function() {
		return(make.predict.types(link = "response"))
	}
)


