#------------------------------------------------------------------------------
#'	(Internal) model.interface class for glm
#'
#'	This reference class contains methods for \code{\link[stats]{glm}} in
#'	\emph{stats} package.
#'
#'	@include model.interface.default.r
#'	@name model.interface.glm-class (stats)
#------------------------------------------------------------------------------
model.interface.glm.class <- R6::R6Class(
	"model.interface.glm", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#'	@method model.interface glm
#'	@export
#'	@describeIn model.interface S3 method for class 'glm'
#------------------------------------------------------------------------------
model.interface.glm <- model.interface.glm.class$new


#------------------------------------------------------------------------------
model.interface.glm.class$set(
	"active", "predict.types",
	function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)
