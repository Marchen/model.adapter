#'	@include model.adapter.default.r
#-------------------------------------------------------------------------------
#'	model.adapter class for rpart
#'
#'	This reference class contains methods for \code{\link[rpart]{rpart}} in 
#'	\emph{rpart} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	rpart関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.rpart <- setRefClass(
	"model.adapter.rpart", contains = "model.adapter"
)


