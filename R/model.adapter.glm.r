#'	@include model.adapter.default.r
#-------------------------------------------------------------------------------
#'	model.adapter class for glm
#'
#'	This reference class contains methods for \code{\link[stats]{glm}} in 
#'	\emph{stats} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	glm関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.glm <- setRefClass(
	"model.adapter.glm", contains = "model.adapter"
)

