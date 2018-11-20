#------------------------------------------------------------------------------
#'	(Internal) model.interface class for MCMCglmm
#'
#'	This reference class contains methods for \code{\link[MCMCglmm]{MCMCglmm}}
#'	in \emph{MCMCglmm} package. Because MCMCglmm object does not have original
#'	data, this class can't obtain data from MCMCglmm objects. Also, the objects
#'	does not have original call, this class cannot obtain call from the
#'	objects. Link and inverse link functions are implimented only for
#'	gaussian, poisson, categorical, multinomial, geometric and exponential
#'	families.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@name model.interface.MCMCglmm-class (MCMCglmm package)
#------------------------------------------------------------------------------
NULL

model.interface.MCMCglmm.class <- R6::R6Class(
	"model.interface.MCMCglmm", inherit = model.interface.default.class
)

model.interface.MCMCglmm <- model.interface.MCMCglmm.class$new


#------------------------------------------------------------------------------
model.interface.MCMCglmm.class$set(
	"public", "get.data",
	function(x, envir, package = "", ...) {
		if (is.call(x)) {
			super$get.data(x, envir, package, ...)
		} else {
			return(NULL)
		}
	}
)


#------------------------------------------------------------------------------
model.interface.MCMCglmm.class$set(
	"public", "get.formula",
	function(x, envir, package = "") {
		if (is.call(x)) {
			return(super$get.formula(x, envir, package))
		} else {
			return(x$Fixed$formula)
		}
	}
)


#------------------------------------------------------------------------------
model.interface.MCMCglmm.class$set(
	"public", "get.family",
	function(x, type = c("character", "family"), envir) {
		# Get family
		if (is.call(x)) {
			family <- extract.family.from.call(x, envir)
		} else {
			family <- x$family[[1]]
		}
		family <- gsub("multinomial.*", "multinomial", family)
		family <- gsub("^categorical$", "multinomial", family)
		# Convert family object to character.
		if (type == "character") {
			if (class(family) != "character") {
				family <- convert.family(family, type)
			}
			return(family)
		}
		if (type == "family") {
			result <- try(convert.family(family, type))
			if (class(result) == "try-error") {
				msg <- sprintf(
					"'%s' family object is not supported by MCMCglmm.", family
				)
				stop(msg)
			} else {
				family <- result
			}
		}
		return(family)
	}
)


#------------------------------------------------------------------------------
model.interface.MCMCglmm.class$set(
	"public", "get.call",
	function(x) {
		return(NULL)
	}
)


#------------------------------------------------------------------------------
model.interface.MCMCglmm.class$set(
	"active", "predict.types",
	function() {
		type <- make.predict.types(
			link = "terms", prob = "response", class = "response"
		)
		return(type)
	}
)


#------------------------------------------------------------------------------
model.interface.MCMCglmm.class$set(
	"public", "get.link",
	function(x, envir) {
		f <- self$get.family(x, "character", envir)
		check.supported.family(f)
		link <- switch(
			f,
			gaussian = identity,
			poisson = log,
			categorical = binomial()$linkfun,
			multinomial = binomial()$linkfun,
			geometric = binomial()$linkfun,
			exponential = function(x) -log(x)
		)
		return(link)
	}
)


#------------------------------------------------------------------------------
model.interface.MCMCglmm.class$set(
	"public", "get.linkinv",
	function(x, envir) {
		f <- self$get.family(x, "character", envir)
		check.supported.family(f)
		link <- switch(
			f,
			gaussian = identity,
			poisson = exp,
			categorical = binomial()$linkinv,
			multinomial = binomial()$linkinv,
			geometric = binomial()$linkinv,
			exponential = function(x) exp(-x)
		)
		return(link)
	}
)


#------------------------------------------------------------------------------
model.interface.MCMCglmm.class$set(
	"public", "get.model.type",
	function(x, envir, package = "", ...) {
		classification <- c("categorical", "ordinal", "threshold")
		family <- self$get.family(x, "character", envir)
		if (family %in% classification | grepl("^multinomial.*", family)) {
			return("classification")
		} else {
			return("regression")
		}
	}
)


#------------------------------------------------------------------------------
#' (Internal) Check if the specified family of MCMCglmm is supported.
#'
#'	@param a character vector of length one specifing
#------------------------------------------------------------------------------
check.supported.family <- function(family) {
	supported <- c(
		"gaussian", "poisson", "categorical", "multinomial", "geometric",
		"exponential"
	)
	if (any(!family %in% supported)) {
		msg <- paste(supported[1:(length(supported) - 1)], collapse = ", ")
		msg <- paste(msg, "and", supported[length(supported)])
		msg <- sprintf(
			"Currently, only MCMCglmm with %s families are supported.
			\nIf you have any information about link function of other families,
			 please teach me.",
			msg
		)
		stop(msg)
	}
}
