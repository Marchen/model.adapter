#-------------------------------------------------------------------------------
#	lm関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for lm
#'
#'	This reference class contains methods for \code{\link[stats]{lm}} in 
#'	\emph{stats} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#-------------------------------------------------------------------------------
model.adapter.lm <- setRefClass(
	"model.adapter.lm", contains = "model.adapter"
)



