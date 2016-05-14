#-------------------------------------------------------------------------------
#	ranger関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for ranger
#'
#'	This reference class contains methods for \code{\link[ranger]{ranger}} in 
#'	\emph{ranger} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#'	@family model.adapter
#'	@export model.adapter.ranger
#'	@exportClass model.adapter.ranger
#-------------------------------------------------------------------------------
model.adapter.ranger <- setRefClass(
	"model.adapter.ranger", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	Initialize call field using match.generic.call().
#	match.generic.call()でcallを初期化。
#-------------------------------------------------------------------------------
model.adapter.ranger$methods(
	initialize = function(x, envir = parent.frame(4L)) {
		x <- substitute(x)
		callSuper(x, envir, caller = "subclass")
		call <<- match.generic.call(call)
	}
)