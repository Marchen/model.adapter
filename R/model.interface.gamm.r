#------------------------------------------------------------------------------
#'	(Internal) model.interface class for gamm
#'
#'	This reference class contains methods for \code{\link[mgcv]{gamm}} in
#'	\emph{mgcv} package.
#'	Note that because an object of gamm does not keep original call,
#'	get.call() function always returns NULL.
#'
#'	@include model.interface.default.r
#'	@name model.interface.gamm-class (mgcv)
#------------------------------------------------------------------------------
model.interface.gamm.class <- R6::R6Class(
	"model.interface.gamm", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#'	@method model.interface gamm
#'	@export
#'	@describeIn model.interface S3 method for class 'gamm'
#------------------------------------------------------------------------------
model.interface.gamm <- model.interface.gamm.class$new


#------------------------------------------------------------------------------
model.interface.gamm.class$set(
	"public", "get.call",
	function(x) {
		# gamm object does not have call.
		return(NULL)
	}
)


#------------------------------------------------------------------------------
model.interface.gamm.class$set(
	"public", "get.family",
	function(x, type = c("character", "family"), envir) {
		if (is.call(x)) {
			return(super$get.family(x, type, envir))
		} else {
			return(super$get.family(x$gam, type, envir))
		}
	}
)


#------------------------------------------------------------------------------
model.interface.gamm.class$set(
	"public", "get.formula",
	function(x, envir, package = "") {
		f <- super$get.formula(x, envir, package)
		if (is.null(f)) {
			f <- x$gam$formula
		}
		return(f)
	}
)


#------------------------------------------------------------------------------
model.interface.gamm.class$set(
	"public", "get.data",
	function(x, envir, package = "", ...) {
		if (is.call(x)){
			return(super$get.data(x, envir, package, ...))
		} else {
			d <- x$gam$model
			var.names <- names(attr(x$gam$terms, "dataClasses"))
			d <- d[var.names]
			return(strip.offset.in.colnames(d))
		}
	}
)


#------------------------------------------------------------------------------
model.interface.gamm.class$set(
	"public", "expand.formula",
	function(
		f, d, specials = c("s", "te", "ti", "t2"), package = "mgcv"
	) {
		super$expand.formula(f, d, specials)
	}
)


#------------------------------------------------------------------------------
model.interface.gamm.class$set(
	"active", "predict.types",
	function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)


#------------------------------------------------------------------------------
model.interface.gamm.class$set(
	"public", "predict",
	function(object, newdata = NULL, type, ...) {
		result <- super$predict(
			object$gam, newdata = newdata, type = type, ...
		)
		return(result)
	}
)


#------------------------------------------------------------------------------
model.interface.gamm.class$set(
	"public", "adjust.offset",
	function(x, envir, package, pred, ...) {
		return(pred)
	}
)
