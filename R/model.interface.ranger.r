#-------------------------------------------------------------------------------
#	ranger関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.interface class for ranger
#'
#'	This reference class contains methods for \code{\link[ranger]{ranger}} in 
#'	\emph{ranger} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.ranger
#'	@exportClass model.interface.ranger
#-------------------------------------------------------------------------------
model.interface.ranger <- setRefClass(
	"model.interface.ranger", contains = "model.interface"
)


