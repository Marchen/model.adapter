#-------------------------------------------------------------------------------
#'	model.adapter class for cforest
#'
#'	This reference class contains methods for \code{\link[party]{cforest}} in 
#'	\emph{party} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	cforest関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.RandomForest <- setRefClass(
	"model.adapter.RandomForest", contains = "model.adapter"
)

model.adapter.RandomForest$methods(
	get.call = function(x) {
		return(NULL)
	}
)

