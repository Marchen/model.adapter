#------------------------------------------------------------------------------
#'	model.interface class for glmer
#'
#'	This reference class contains methods for \code{\link[lme4]{glmer}} in
#'	\emph{lme4} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.glmerMod
#'	@exportClass model.interface.glmerMod
#------------------------------------------------------------------------------
model.interface.glmerMod <- setRefClass(
	"model.interface.glmerMod", contains = "model.interface"
)


#------------------------------------------------------------------------------
model.interface.glmerMod$methods(
	get.data = function(x, envir, package = "", ...) {
		if (is.call(x)){
			return(callSuper(x, envir, package, ...))
		} else {
			d <- x@frame
			attr(d, "terms") <- NULL
			return(d)
		}
	}
)


#------------------------------------------------------------------------------
model.interface.glmerMod$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)
