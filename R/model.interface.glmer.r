#------------------------------------------------------------------------------
#'	(Internal) model.interface class for glmer
#'
#'	This reference class contains methods for \code{\link[lme4]{glmer}} in
#'	\emph{lme4} package.
#'
#'	@include model.interface.default.r
#'	@family model.interface classes
#'	@name model.interface.glmerMod-class (lme4)
#------------------------------------------------------------------------------
NULL

model.interface.glmerMod.class <- R6::R6Class(
	"model.interface.glmerMod", inherit = model.interface.default.class
)

model.interface.glmerMod <- model.interface.glmerMod.class$new


#------------------------------------------------------------------------------
model.interface.glmerMod.class$set(
	"public", "get.data",
	function(x, envir, package = "", ...) {
		if (is.call(x)){
			return(super$get.data(x, envir, package, ...))
		} else {
			d <- x@frame
			attr(d, "terms") <- NULL
			return(d)
		}
	}
)


#------------------------------------------------------------------------------
model.interface.glmerMod.class$set(
	"active", "predict.types",
	function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)
