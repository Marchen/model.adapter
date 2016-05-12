#-------------------------------------------------------------------------------
#	gam関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for gam
#'
#'	This reference class contains methods for \code{\link[mgcv]{gam}} in 
#'	\emph{mgcv} package and \code{\link[gam]{gam}} in \emph{gam} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#-------------------------------------------------------------------------------
model.adapter.gam <- setRefClass(
	"model.adapter.gam", contains = "model.adapter"
)

