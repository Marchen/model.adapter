#-------------------------------------------------------------------------------
#'	model.adapter class for ranger
#'
#'	This reference class contains methods for \code{\link[ranger]{ranger}} in 
#'	\emph{ranger} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	ranger関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.ranger <- setRefClass(
	"model.adapter.ranger", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	Initialize call field using match.generic.call().
#	match.generic.call()でcallを初期化。
#-------------------------------------------------------------------------------
model.adapter.ranger$methods(
	initialize = function(x) {
		callSuper(x)
		# Initialize call field. / callフィールドの初期化。
		call <<- match.generic.call(call)
	}
)