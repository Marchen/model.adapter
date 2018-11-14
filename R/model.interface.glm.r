#------------------------------------------------------------------------------
#'	(Internal) model.interface class for glm
#'
#'	This reference class contains methods for \code{\link[stats]{glm}} in
#'	\emph{stats} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#------------------------------------------------------------------------------
model.interface.glm <- setRefClass(
	"model.interface.glm", contains = "model.interface"
)


#------------------------------------------------------------------------------
model.interface.glm$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)
