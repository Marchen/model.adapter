#------------------------------------------------------------------------------
#'	(Internal) model.interface class for gamm
#'
#'	This reference class contains methods for \code{\link[mgcv]{gamm}} in
#'	\emph{mgcv} package.
#'	Note that because an object of gamm does not keep original call,
#'	get.call() function always returns NULL. Also, when an instance of this
#'	class is made from model object, 'call' field is always call("<undef>").
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#------------------------------------------------------------------------------
model.interface.gamm <- setRefClass(
	"model.interface.gamm", contains = "model.interface"
)


#------------------------------------------------------------------------------
model.interface.gamm$methods(
	get.call = function(x) {
		return(NULL)
	}
)


#------------------------------------------------------------------------------
model.interface.gamm$methods(
	get.family = function(x, type = c("character", "family"), envir) {
		if (is.call(x)) {
			return(callSuper(x, type, envir))
		} else {
			return(callSuper(x$gam, type, envir))
		}
	}
)


#------------------------------------------------------------------------------
model.interface.gamm$methods(
	get.formula = function(x, envir, package = "") {
		f <- callSuper(x, envir, package)
		if (is.null(f)) {
			f <- x$gam$formula
		}
		return(f)
	}
)


#------------------------------------------------------------------------------
model.interface.gamm$methods(
	get.data = function(x, envir, package = "", ...) {
		if (is.call(x)){
			return(callSuper(x, envir, package, ...))
		} else {
			d <- x$gam$model
			var.names <- names(attr(x$gam$terms, "dataClasses"))
			d <- d[var.names]
			return(d)
		}
	}
)


#------------------------------------------------------------------------------
model.interface.gamm$methods(
	expand.formula = function(
		f, d, specials = c("s", "te", "ti", "t2"), package = "mgcv"
	) {
		callSuper(f, d, specials)
	}
)


#------------------------------------------------------------------------------
model.interface.gamm$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)


#------------------------------------------------------------------------------
model.interface.gamm$methods(
	predict = function(object, newdata = NULL, type, ...) {
		return(callSuper(object$gam, newdata = newdata, type = type, ...))
	}
)
