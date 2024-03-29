#------------------------------------------------------------------------------
#' (Internal) model.interface class for lme
#'
#' This reference class contains methods for \code{\link[nlme]{lme}} in
#' \emph{nlme} package.
#'
#' @include model.interface.default.r
#' @name model.interface.lme-class (nlme)
#------------------------------------------------------------------------------
model.interface.lme.class <- R6::R6Class(
    "model.interface.lme", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#' @method model.interface lme
#' @export
#' @describeIn model.interface S3 method for class 'lme'
#------------------------------------------------------------------------------
model.interface.lme <- model.interface.lme.class$new


#------------------------------------------------------------------------------
model.interface.lme.class$set(
    "public", "get.call",
    function(x) {
        call.list <- as.list(x$call)
        call.list[[1]] <- substitute(lme)
        return(as.call(call.list))
    }
)


#------------------------------------------------------------------------------
model.interface.lme.class$set(
    "public", "get.formula",
    function(x, envir, package = "") {
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
model.interface.lme.class$set(
    "public", "predict",
    function(object, newdata, ...) {
        # set level = 0 to marginalize random effect.
        if (is.null(newdata)) {
            fit <- stats::predict(object, level = 0, ...)
        } else {
            fit <- stats::predict(object, newdata, level = 0, ...)
        }
        return(fit)
    }
)
