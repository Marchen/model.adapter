#-------------------------------------------------------------------------------
#	svm関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for svm
#'
#'	This reference class contains methods for \code{\link[e1071]{svm}} in 
#'	\emph{e1071} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#-------------------------------------------------------------------------------
model.adapter.svm <- setRefClass(
	"model.adapter.svm", contains = "model.adapter"
)

