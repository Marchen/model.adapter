#------------------------------------------------------------------------------
#'	model.interface class for glmmML
#'
#'	This reference class contains methods for \code{\link[glmmML]{glmmML}} in
#'	\emph{glmmML} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.glmmML
#'	@exportClass model.interface.glmmML
#------------------------------------------------------------------------------
model.interface.glmmadmb <- setRefClass(
	"model.interface.glmmadmb", contains = "model.interface"
)


#------------------------------------------------------------------------------
model.interface.glmmadmb$methods(
	get.family = function(x, type = c("character", "family"), envir) {
		# Get family character.
		if (is.call(x)) {
			family <- family.from.call(x, envir)
		} else {
			family <- x$family
		}
		family <- gsub("^binom$", "binomial", family)
		family <- gsub("^gamma$", "Gamma", family)
		if (type == "character") {
			return(family)
		}
		# Convert family character to family object.
		result <- try(format.family(family, type))
		if (class(result) == "try-error") {
			msg <- sprintf(
				"'%s' family object is not supported by glmmadmb.", family
			)
			stop(msg)
		}
		return(result)
	}
)


#------------------------------------------------------------------------------
model.interface.glmmadmb$methods(
	get.data = function(x, envir, package = "", ...) {
		if (is.call(x)) {
			return(callSuper(x, envir, package, ...))
		} else {
			d <- x$frame
			attr(d, "terms") <- NULL
			return(d)
		}
	}
)


#------------------------------------------------------------------------------
model.interface.glmmadmb$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)


#------------------------------------------------------------------------------
model.interface.glmmadmb$methods(
	predict = function(object, newdata, type, ...) {
		# Change 'fixed' field of object to handle '.' in the formula.
		object$fixed <- .self$expand.formula(object$fixed, object$frame)
		callSuper(object, newdata, type, ...)
	}
)
