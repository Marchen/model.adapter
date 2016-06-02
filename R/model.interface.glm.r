#-------------------------------------------------------------------------------
#	glm関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.interface class for glm
#'
#'	This reference class contains methods for \code{\link[stats]{glm}} in 
#'	\emph{stats} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.glm
#'	@exportClass model.interface.glm
#-------------------------------------------------------------------------------
model.interface.glm <- setRefClass(
	"model.interface.glm", contains = "model.interface"
)

