#-------------------------------------------------------------------------------
#	rpart関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.interface class for rpart
#'
#'	This reference class contains methods for \code{\link[rpart]{rpart}} in
#'	\emph{rpart} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.rpart
#'	@exportClass model.interface.rpart
#-------------------------------------------------------------------------------
model.interface.rpart <- setRefClass(
	"model.interface.rpart", contains = "model.interface"
)


