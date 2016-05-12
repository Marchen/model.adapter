#'	@include model.adapter.default.r
#-------------------------------------------------------------------------------
#'	model.adapter class for svm
#'
#'	This reference class contains methods for \code{\link[e1071]{svm}} in 
#'	\emph{e1071} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	svm関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.svm <- setRefClass(
	"model.adapter.svm", contains = "model.adapter"
)

