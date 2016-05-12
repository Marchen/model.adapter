#-------------------------------------------------------------------------------
#	gbm関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for gbm
#'
#'	This reference class contains methods for \code{\link[gbm]{gbm}} in 
#'	\emph{gbm} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#-------------------------------------------------------------------------------
model.adapter.gbm <- setRefClass(
	"model.adapter.gbm", contains = "model.adapter"
)



