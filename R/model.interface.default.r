#------------------------------------------------------------------------------
#'	(Internal) Initialize model.interface class.
#'
#'	This function makes an object of a derived class of \emph{model.interface}
#'	class that abstracts differences in specifications of supported modeling
#'	functions.
#'
#'	@param x
#'		an object of supported models.
#'		This is only used for dispatching S3 methods.
#'
#'	@return
#'		an object of derived class of \code{\link{model.interface-class}}.
#'
#'	@section Adding support for new function:
#'		To be continued...
#------------------------------------------------------------------------------
model.interface <- function(x = NULL) {
	UseMethod("model.interface")
}


#------------------------------------------------------------------------------
#'	(Internal) An interface for model.adapter and statistical/machine learning models.
#'
#'	This class provides unified interface for calls/objects of
#'	statistical/machine learning models.
#'	\code{\link{model.adapter-class}{model.adapter}} internally using object
#'	of this class to get information from model calls/objects.
#'
#'	@field predict.types
#'		a named character vector representing the conversion table of shared
#'		type of prediction used for predict() method. Currently, this vector has
#'		four elements named "response", "link", "prob" and "class" and
#'		each element represents compatible type of character for the model.
#'
#'	@section Methods:
#'
#'	\strong{\code{get.family(x, type = c("character", "family"), envir)}}
#'
#'		Get family from the model.
#'
#'		\describe{
#'			\item{\code{x}}{call or model object.}
#'			\item{\code{type = c("character", "family")}}{
#'				a character literal specifying the type of data returned.
#'				If "character", this returns character vector of family name.
#'				If "family", this returns \code{\link{family}} object.
#'				For some family specific for certain model (e.g., categorical
#'				family of MCMCglmm), their family object is not implimented.
#'				For such family, this method raise stop error.
#'			}
#'			\item{\code{envir}}{environment where x is evaluated.}
#'		}
#'
#'	\strong{\code{get.call(x)}}
#'
#'		This method returns call by which the object is made. If call is not
#'		available, this returns NULL. To distinguish the returned value of NULL
#'		is intended action or not, inherited classes are encouraged to
#'		inherit this method to explicitly return NULL if x does not have call.
#'
#'		\describe{
#'			\item{\code{x}}{call or model object.}
#'		}
#'
#'	\strong{\code{get.data(x, envir, package = "", ...)}}
#'
#'		Get a data.frame containing the data used for modeling.
#'		If data is not available this method returns NULL.
#'
#'		\describe{
#'			\item{\code{x}}{
#'				a model object/call from which data is extracted.
#'			}
#'			\item{\code{envir}}{an environment in which call is evaluated.}
#'		}
#'
#'	\strong{\code{get.formula(x, envir, package = "")}}
#'
#'		Extract formula from model object/call.
#'		If couldn't retrieve formula from \code{x}, this method returns NULL.
#'
#'		\describe{
#'			\item{\code{x}}{
#'				a model object/call from which formula is extracted.
#'			}
#'			\item{\code{envir}}{
#'				an environment in which call in x is evaluated.
#'			}
#'			\item{\code{package}}{
#'				name of the package having the modeling function.
#'			}
#'		}
#'
#'	\strong{\code{expand.formula(f, d, specials = NULL, package = NULL)}}
#'
#'		Expand . in formula.
#'
#'		\describe{
#'			\item{\code{f}}{a formula to expand.}
#'			\item{\code{d}}{a data.frame used to expand . in formula.}
#'			\item{\code{specials = NULL}}{
#'				special characterss passed to
#'				\code{\link[stats]{terms.formula}}.
#'			}
#'			\item{\code{package = NULL}}{
#'				a character literal of package name having the model function.
#'			}
#'		}
#'
#'	\strong{\code{predict(object, newdata, type, random, ...)}}
#'
#'		Calculate predictions.
#'
#'		\describe{
#'			\item{\code{object}}{a model object used for prediction.}
#'			\item{\code{newdata}}{
#'				a data.frame containing data used for prediction.
#'			}
#'			\item{\code{type}}{
#'				the type of prediciton. This should be a type specific for
#'				each modeling functions.
#'			}
#'			\item{\code{random = ~0}}{
#'				the random effect to use.
#'				Tentatively, ~0 means don't use random effects.
#'			}
#'			\item{\code{...}}{other variables passed to predict methods.}
#'		}
#'
#'	\strong{\code{get.link(x, envir)}}
#'
#'		Get link function. If the model does not have link function, this
#'		function returns \code{\link[base]{identity}} function.
#'
#'		\describe{
#'			\item{\code{x}}{
#'				an object of statistical model or a call of model function.
#'			}
#'			\item{\code{envir}}{
#'				an environment where call in \code{x} is evaluated.
#'			}
#'		}
#'
#'	\strong{\code{get.linkinv(x, envir)}}
#'
#'		Get inverse function of link function. If the model does not have
#'		link function, this function returns \code{\link[base]{identity}}
#'		function.
#'
#'		\describe{
#'			\item{\code{x}}{
#'				an object of statistical model or a call of model function.
#'			}
#'			\item{\code{envir}}{
#'				an environment where call in \code{x} is evaluated.
#'			}
#'		}
#'
#'	\strong{\code{get.model.type(x, envir, package = "", ...)}}
#'
#'		Return a character vector specifying model type
#'		(regression or classification).
#'		If the model is regression model, it returns 'regression'.
#'		If the model is classification model, it returns 'classification'.
#'
#'		\describe{
#'			\item{\code{x}}{
#'				an object of statistical model or a call of model function.
#'			}
#'			\item{\code{envir}}{
#'				an environment where call in \code{x} is evaluated.
#'			}
#'		}
#'
#'	@family model.interface classes
#'	@name model.interface-class
#------------------------------------------------------------------------------
NULL

model.interface.default.class <- R6::R6Class(
	"model.interface"
)
model.interface.default <- model.interface.default.class$new


#------------------------------------------------------------------------------
#	Initialize.
#------------------------------------------------------------------------------
model.interface.default.class$set(
	"public", "initialize", function(...){}
)


#------------------------------------------------------------------------------
#	Get family of the model.
#------------------------------------------------------------------------------
model.interface.default.class$set(
	"public", "get.family",
	function(x, type = c("character", "family"), envir) {
		if (is.call(x)) {
			family <- extract.family.from.call(x, envir)
		} else {
			family <- try(stats::family(x), TRUE)
			if (class(family) == "try-error") {
				# if x$family is NULL, family should be NULL.
				if (isS4(x)) {
					if ("family" %in% slotNames(x)) {
						family <- x@family
					} else {
						return(NULL)
					}
				} else {
					family <- x$family
				}
			}
		}
		if (!is.null(family)) {
			return(convert.family(family, type))
		} else {
			return(NULL)
		}
	}
)


#------------------------------------------------------------------------------
#	Get model call.
#------------------------------------------------------------------------------
model.interface.default.class$set(
	"public", "get.call",
	function(x) {
		if (isS4(x)) {
			result <- x@call
		} else {
			result <- x$call
			if (is.null(result)) {
				warning("get.call() implicitly returns NULL. Is it intended?")
			}
		}
		return(result)
	}
)


#------------------------------------------------------------------------------
#	Get data of the model.
#------------------------------------------------------------------------------
model.interface.default.class$set(
	"public", "get.data",
	function(x, envir, package = "", ...) {
		if (is.call(x)) {
			d <- eval(x$data, envir)
		} else {
			if (isS4(x)) {
				d <- x@data
			} else {
				d <- x$data
			}
			if (is.null(d)) {
				# When couldn't retrieve data from object, get it from call.
				cl <- match.generic.call(self$get.call(x), envir, package)
				d <- eval(cl$data, envir)
			}
		}
		return(d)
	}
)


#------------------------------------------------------------------------------
#	Extract formula from model object/call.
#------------------------------------------------------------------------------
model.interface.default.class$set(
	"public", "get.formula",
	function(x, envir, package = "") {
		if (is.object(x)) {
			if (isS4(x)) {
				f <- eval(x@call$formula, envir)
			} else {
				f <- eval(x$call$formula, envir)
			}
			if (is.null(f)) {
				f <- try(formula(x, env = envir), silent = TRUE)
				if (class(f) == "try-error") {
					f <- NULL
				}
			}
		} else {
			x <- match.generic.call(x, envir, package)
			if (!is.null(x$formula)) {
				f <- eval(x$formula, envir)
			} else {
				args <- lapply(as.list(x), eval, envir = envir)
				f <- args[sapply(args, is.formula)][[1]]
			}
		}
		# Because MASS::stepAIC converts formula field of lm, glm, lme
		# object to terms object, use formula() to convert terms to
		# formula.
		if (!is.null(f)) {
			f <- formula(f)
			# Remove environment with formula.
			f <- as.formula(as.character(deparse(f)))
		}
		return(f)
	}
)


#------------------------------------------------------------------------------
#	Expand . in formula.
#------------------------------------------------------------------------------
model.interface.default.class$set(
	"public", "expand.formula",
	function(f, d, specials = NULL, package = NULL) {
		result <- terms(f, data = d, specials = specials)
		attributes(result) <- NULL
		result <- as.formula(result)
		return(result)
	}
)


#------------------------------------------------------------------------------
#	Return a character vector representing conversion table of 'type'
#	argument of predict() method.
#------------------------------------------------------------------------------
model.interface.default.class$set(
	"active", "predict.types",
	function() {
		return(make.predict.types())
	}
)


#------------------------------------------------------------------------------
#	Calculate predictions.
#------------------------------------------------------------------------------
model.interface.default.class$set(
	"public", "predict",
	function(object, newdata, type, random, ...) {
		if (is.null(newdata)) {
			pred <- stats::predict(object, type = type, ...)
		} else {
			pred <- stats::predict(object, newdata = newdata, type = type, ...)
		}
		return(pred)
	}
)


#------------------------------------------------------------------------------
#	Get link function from the model.
#------------------------------------------------------------------------------
model.interface.default.class$set(
	"public", "get.link",
	function(x, envir) {
		f <- self$get.family(x, "family", envir)
		if (!is.null(f)) {
			return(f$linkfun)
		} else {
			return(identity)
		}
	}
)


#------------------------------------------------------------------------------
#	Get inverse link function of the model.
#------------------------------------------------------------------------------
model.interface.default.class$set(
	"public", "get.linkinv",
	function(x, envir) {
		f <- self$get.family(x, "family", envir)
		if (!is.null(f)) {
			return(f$linkinv)
		} else {
			return(identity)
		}
	}
)


#------------------------------------------------------------------------------
#	Return a character vector specifying model type
#	(regression or classification).
#------------------------------------------------------------------------------
model.interface.default.class$set(
	"public", "get.model.type",
	function(x, envir, package = "", ...) {
		f <- self$get.family(x, type = "character", envir)
		if (is.null(f)) {
			d <- self$get.data(x, envir, package, ...)
			response <- model.response(
				model.frame(self$get.formula(x, envir, package), data = d)
			)
			if (is(response, "factor")) {
				return("classification")
			} else {
				return("regression")
			}
		}
		# For glm and gam, following families are treated as classification.
		# Other families are regarded as regression.
		classification.families <- c(
			"binomial", "quasibinomial", "negbin", "ocat", "nb",
			"betar", "cox.ph"
		)
		if (f %in% classification.families) {
			return("classification")
		}
		return("regression")
	}
)
