#------------------------------------------------------------------------------
#'	model.interface class for lme
#'
#'	This reference class contains methods for \code{\link[nlme]{lme}} in
#'	\emph{nlme} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.lme
#'	@exportClass model.interface.lme
#------------------------------------------------------------------------------
model.interface.lme <- setRefClass(
	"model.interface.lme", contains = "model.interface"
)


#------------------------------------------------------------------------------
model.interface.lme$methods(
	get.call = function(x) {
		call.list <- as.list(x$call)
		call.list[[1]] <- substitute(lme)
		return(as.call(call.list))
	}
)


#------------------------------------------------------------------------------
model.interface.lme$methods(
	get.formula = function(x, envir, package = "") {
		# Get call and convert it to a list.
		if (is.object(x)) {
			cl <- x$call
		} else {
			cl <- x
		}
		cl <- match.call(lme, cl)
		args <- lapply(as.list(cl), eval, envir = envir)
		# Because MASS::stepAIC converts formula field of lm, glm, lme
		# object to terms object, use formula() to convert terms to
		# formula.
		return(formula(args$fixed))
	}
)


#------------------------------------------------------------------------------
model.interface.lme$methods(
	predict = function(object, newdata, ...) {
		# set level = 0 to marginalize random effect.
		if (is.null(newdata)) {
			fit <- stats::predict(object, level = 0, ...)
		} else {
			fit <- stats::predict(object, newdata, level = 0, ...)
		}
		return(fit)
	}
)


#------------------------------------------------------------------------------
#	モデルオブジェクトから説明変数の係数を取得する。
#------------------------------------------------------------------------------
model.interface.lme$methods(
	get.fixed = function(object, intercept = TRUE) {
		result <- object$coefficients$fixed
		if (!intercept) {
			result <- result[names(result) != "(Intercept)"]
		}
		return(result)
	}
)


#------------------------------------------------------------------------------
#	モデルオブジェクトから切片の推定値を取得する。
#------------------------------------------------------------------------------
model.interface.lme$methods(
	get.intercept = function(object) {
		return(object$coefficients$fixed["(Intercept)"])
	}
)


