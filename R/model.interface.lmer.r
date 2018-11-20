#------------------------------------------------------------------------------
#'	(Internal) model.interface class for lmer
#'
#'	This reference class contains methods for \code{\link[lme4]{lmer}} in
#'	\emph{lme4} package.
#'
#'	@include model.interface.default.r
#'	@name model.interface.lmerMod-class (lme4)
#------------------------------------------------------------------------------
model.interface.lmerMod.class <- R6::R6Class(
	"model.interface.lmerMod", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#'	@method model.interface lmerMod
#'	@export
#'	@describeIn model.interface S3 method for class 'lmerMod'
#------------------------------------------------------------------------------
model.interface.lmerMod <- model.interface.lmerMod.class$new


#------------------------------------------------------------------------------
model.interface.lmerMod.class$set(
	"public", "get.data",
	function(x, envir, package = "", ...) {
		interface <- model.interface.glmerMod(x)
		return(interface$get.data(x, envir, package, ...))
	}
)


#------------------------------------------------------------------------------
model.interface.lmerMod.class$set(
	"public", "get.family",
	function(x, type = c("character", "family"), envir) {
		return(convert.family("gaussian", type))
	}
)


#------------------------------------------------------------------------------
model.interface.lmerMod.class$set(
	"active", "predict.types",
	function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)
