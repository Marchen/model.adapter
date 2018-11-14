#------------------------------------------------------------------------------
#'	(Internal) model.interface class for glmmML
#'
#'	This reference class contains methods for \code{\link[glmmML]{glmmML}} in
#'	\emph{glmmML} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#------------------------------------------------------------------------------
model.interface.glmmML <- setRefClass(
	"model.interface.glmmML", contains = "model.interface"
)


#------------------------------------------------------------------------------
model.interface.glmmML$methods(
	get.family = function(x, type = c("character", "family"), envir) {
		if (is.call(x)) {
			family <- family.from.call(x, envir)
		} else {
			family <- x$call$family
		}
		return(format.family(family, type))
	}
)


#------------------------------------------------------------------------------
model.interface.glmmML$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)


#-------------------------------------------------------------------------------
#'	predict method for glmmML object.
#'
#'	This is a predict method for \emph{glmmML} object.
#'
#'	@param object a glmmML object.
#'	@param newdata
#'		a data.frame containing data used for prediction. If missing, the data
#'		used for the modeling is used. However, because glmmML object does not
#'		contain original data used for the modeling, retrieving the data used
#'		for the modeling may fail if the data is removed from workspace.
#'	@param type
#'		type of prediction. Default is scale of response variable.
#'		If "link" is specified, prediction is of link scale is calculated.
#'	@param conditional
#'		if FALSE, marginal (using fixed terms only and unconditioned to random
#'		effect) predicted values are calculated. If TRUE, predicted values
#'		conditioned to the random effect is calculated.
#'	@param ... further arguments passed to other methods.
#-------------------------------------------------------------------------------
predict.glmmML <- function(
	object, newdata, type = c("response", "link"), conditional = FALSE, ...
) {
	if (missing(newdata)) {
		newdata <- eval(object$call$data)
	}
	type = match.arg(type)
	design.matrix <- model.matrix(object, data = newdata)
	result <- design.matrix %*% coef(object)
	# Add random intercept.
	if (conditional) {
		cluster <- eval(object$call$cluster, newdata)
		result <- result + object$posterior.mode[as.numeric(cluster)]
	}
	if (type == "link") {
		return(result)
	} else {
		family <- format.family(object$call$family, type = "family")
		return(family$linkinv(result))
	}
}
