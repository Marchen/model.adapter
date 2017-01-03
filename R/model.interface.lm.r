#-------------------------------------------------------------------------------
#	lm関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.interface class for lm
#'
#'	This reference class contains methods for \code{\link[stats]{lm}} in
#'	\emph{stats} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.lm
#'	@exportClass model.interface.lm
#-------------------------------------------------------------------------------
model.interface.lm <- setRefClass(
	"model.interface.lm", contains = "model.interface"
)



