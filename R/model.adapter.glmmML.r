#-------------------------------------------------------------------------------
#'	model.adapter class for glmmML
#'
#'	This reference class contains methods for \code{\link[glmmML]{glmmML}} in 
#'	\emph{glmmML} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	glmmML関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.glmmML <- setRefClass(
	"model.adapter.glmmML", contains = "model.adapter"
)


