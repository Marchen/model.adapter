#------------------------------------------------------------------------------
#'	(Internal) model.interface class for glmmTMB
#'
#'	This reference class contains methods for \code{\link[glmmTMB]{glmmTMB}}
#'	in \emph{glmmTMB} package.
#'
#'	@include model.interface.default.r
#'	@name model.interface.glmmTMB-class (glmmTMB)
#------------------------------------------------------------------------------
model.interface.glmmTMB.class <- R6::R6Class(
	"model.interface.glmmTMB", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#'	@method model.interface glmmTMB
#'	@export
#'	@describeIn model.interface S3 method for class 'glmmTMB'
#------------------------------------------------------------------------------
model.interface.glmmTMB <- model.interface.glmmTMB.class$new


#------------------------------------------------------------------------------
model.interface.glmmTMB.class$set(
	"active", "predict.types",
	function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)
