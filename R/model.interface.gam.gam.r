#------------------------------------------------------------------------------
#'	(Internal) model.interface class for gam in gam package
#'
#'	This reference class contains methods for \code{\link[gam]{gam}} in
#'	\emph{gam} package.
#'
#'	@include model.interface.default.r
#'	@name model.interface.Gam-class (gam)
#------------------------------------------------------------------------------
model.interface.Gam.class <- R6::R6Class(
	"model.interface.Gam", inherit = model.interface.default.class
)

model.interface.Gam <- model.interface.Gam.class$new


#------------------------------------------------------------------------------
model.interface.Gam.class$set(
	"public", "expand.formula",
	function(f, d, specials = NULL, package = "mgcv") {
		require(gam)
		# Version > 1.15
		return(super$expand.formula(f, d, specials = gam.smooth.list$slist))
	}
)


#------------------------------------------------------------------------------
model.interface.Gam.class$set(
	"active", "predict.types",
	function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)
