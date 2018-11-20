#------------------------------------------------------------------------------
#'	(Internal) model.interface class for glmmML
#'
#'	This reference class contains methods for \code{\link[glmmML]{glmmML}} in
#'	\emph{glmmML} package.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@name model.interface.glmmadmb-class (glmmADMB package)
#------------------------------------------------------------------------------
NULL

model.interface.glmmadmb.class <- R6::R6Class(
	"model.interface.glmmadmb", inherit = model.interface.default.class
)

model.interface.glmmadmb <- model.interface.glmmadmb.class$new


#------------------------------------------------------------------------------
model.interface.glmmadmb.class$set(
	"public", "get.family",
	function(x, type = c("character", "family"), envir) {
		# Get family character.
		if (is.call(x)) {
			family <- extract.family.from.call(x, envir)
		} else {
			family <- x$family
		}
		family <- gsub("^binom$", "binomial", family)
		family <- gsub("^gamma$", "Gamma", family)
		if (type == "character") {
			return(family)
		}
		# Convert family character to family object.
		result <- try(convert.family(family, type))
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
model.interface.glmmadmb.class$set(
	"public", "get.data",
	function(x, envir, package = "", ...) {
		if (is.call(x)) {
			return(super$get.data(x, envir, package, ...))
		} else {
			d <- x$frame
			attr(d, "terms") <- NULL
			return(d)
		}
	}
)


#------------------------------------------------------------------------------
model.interface.glmmadmb.class$set(
	"active", "predict.types",
	function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)


#------------------------------------------------------------------------------
model.interface.glmmadmb.class$set(
	"public", "predict",
	function(object, newdata, type, ...) {
		# Change 'fixed' field of object to handle '.' in the formula.
		object$fixed <- self$expand.formula(object$fixed, object$frame)
		super$predict(object, newdata, type, ...)
	}
)
