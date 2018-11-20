#------------------------------------------------------------------------------
#'	(Internal) model.interface class for glmmML
#'
#'	This reference class contains methods for \code{\link[glmmML]{glmmML}} in
#'	\emph{glmmML} package. 'predict' method is implimented in this package,
#'	rather than \emph{glmmML} package.
#'
#'	@include model.interface.default.r
#'	@family model.interface classes
#'	@name model.interface.glmmML-class
#------------------------------------------------------------------------------
NULL

model.interface.glmmML.class <- R6::R6Class(
	"model.interface.glmmML", inherit = model.interface.default.class
)

model.interface.glmmML <- model.interface.glmmML.class$new


#------------------------------------------------------------------------------
model.interface.glmmML.class$set(
	"public", "get.family",
	function(x, type = c("character", "family"), envir) {
		if (is.call(x)) {
			family <- extract.family.from.call(x, envir)
		} else {
			family <- x$call$family
		}
		return(convert.family(family, type))
	}
)


#------------------------------------------------------------------------------
model.interface.glmmML.class$set(
	"active", "predict.types",
	function() {
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
		family <- convert.family(object$call$family, type = "family")
		return(family$linkinv(result))
	}
}
