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


#-------------------------------------------------------------------------------
#	Initialize call field using match.generic.call().
#	match.generic.call()でcallを初期化。
#-------------------------------------------------------------------------------
#model.interface.ranger$methods(
	#initialize = function(x, envir = parent.frame(4L), ...) {
		#x <- substitute(x)
		#callSuper(x, envir, caller = "subclass")
		#call <<- match.generic.call(call)
	#}
#)