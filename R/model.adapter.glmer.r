#-------------------------------------------------------------------------------
#'	model.adapter class for glmer
#'
#'	This reference class contains methods for \code{\link[lme4]{glmer}} in 
#'	\emph{lme4} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	glmer関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.glmer <- setRefClass(
	"model.adapter.glmer", contains = "model.adapter"
)
