#-------------------------------------------------------------------------------
#	svm関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.interface class for svm
#'
#'	This reference class contains methods for \code{\link[e1071]{svm}} in 
#'	\emph{e1071} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.svm
#'	@exportClass model.interface.svm
#-------------------------------------------------------------------------------
model.interface.svm <- setRefClass(
	"model.interface.svm", contains = "model.interface"
)

