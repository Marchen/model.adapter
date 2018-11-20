#------------------------------------------------------------------------------
#'	(Internal) model.interface class for gam
#'
#'	This reference class contains methods for \code{\link[mgcv]{gam}} in
#'	\emph{mgcv} package.
#'
#'	@include model.interface.default.r
#'	@family model.interface classes
#'	@name model.interface.gam-class(mgcv)
#------------------------------------------------------------------------------
NULL

# gam in mgcv package.
model.interface.gam.class <- R6::R6Class(
	"model.interface.gam", inherit = model.interface.default.class
)

model.interface.gam <- model.interface.gam.class$new


#------------------------------------------------------------------------------
model.interface.gam.class$set(
	"public", "expand.formula",
	function(f, d, specials = NULL, package = "mgcv") {
		return(super$expand.formula(f, d, specials = c("s", "te", "ti", "t2")))
	}
)


#------------------------------------------------------------------------------
model.interface.gam.class$set(
	"active", "predict.types",
	function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)
