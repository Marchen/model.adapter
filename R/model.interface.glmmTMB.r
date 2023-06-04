#------------------------------------------------------------------------------
#' (Internal) model.interface class for glmmTMB
#'
#' This reference class contains methods for \code{\link[glmmTMB]{glmmTMB}}
#' in \emph{glmmTMB} package.
#'
#' @include model.interface.default.r
#' @name model.interface.glmmTMB-class (glmmTMB)
#------------------------------------------------------------------------------
model.interface.glmmTMB.class <- R6::R6Class(
    "model.interface.glmmTMB", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#' @method model.interface glmmTMB
#' @export
#' @describeIn model.interface S3 method for class 'glmmTMB'
#------------------------------------------------------------------------------
model.interface.glmmTMB <- model.interface.glmmTMB.class$new


#------------------------------------------------------------------------------
model.interface.glmmTMB.class$set(
    "active", "predict.types",
    function() {
        return(make.predict.types(prob = "response", class = "response"))
    }
)


#------------------------------------------------------------------------------
#   Calculate predictions.
#------------------------------------------------------------------------------
model.interface.glmmTMB.class$set(
    "public", "predict",
    function(object, newdata, type, random, interval, ...) {
        if (is.null(newdata)) {
            pred <- stats::predict(object, type = type, re.form=...)
        } else {
            pred <- stats::predict(object, newdata = newdata, type = type, ...)
        }
        return(pred)
    }
)

