#-------------------------------------------------------------------------------
#	glm関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for glm
#'
#'	This reference class contains methods for \code{\link[stats]{glm}} in 
#'	\emph{stats} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#'	@family model.adapter
#'	@export model.adapter.glm
#'	@exportClass model.adapter.glm
#-------------------------------------------------------------------------------
model.adapter.glm <- setRefClass(
	"model.adapter.glm", contains = "model.adapter"
)

