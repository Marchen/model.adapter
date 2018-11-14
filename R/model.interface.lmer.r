#------------------------------------------------------------------------------
#'	(Internal) model.interface class for lmer
#'
#'	This reference class contains methods for \code{\link[lme4]{lmer}} in
#'	\emph{lme4} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#------------------------------------------------------------------------------
model.interface.lmerMod <- setRefClass(
	"model.interface.lmerMod", contains = "model.interface"
)


#------------------------------------------------------------------------------
model.interface.lmerMod$methods(
	get.data = function(x, envir, package = "", ...) {
		interface <- model.interface.glmerMod(x)
		return(interface$get.data(x, envir, package, ...))
	}
)


#------------------------------------------------------------------------------
model.interface.lmerMod$methods(
	get.family = function(x, type = c("character", "family"), envir) {
		return(format.family("gaussian", type))
	}
)


#------------------------------------------------------------------------------
model.interface.lmerMod$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)
