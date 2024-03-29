#------------------------------------------------------------------------------
#' (Internal) model.interface class for svm
#'
#' This reference class contains methods for \code{\link[e1071]{svm}} in
#' \emph{e1071} package.
#'
#' @include model.interface.default.r
#' @name model.interface.svm-class (svm)
#------------------------------------------------------------------------------
model.interface.svm.class <- R6::R6Class(
    "model.interface.svm", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#' @method model.interface svm
#' @export
#' @describeIn model.interface S3 method for class 'svm'
#------------------------------------------------------------------------------
model.interface.svm <- model.interface.svm.class$new


#------------------------------------------------------------------------------
model.interface.svm.class$set(
    "public", "predict",
    function(object, newdata = NULL, type, ...) {
        if (is.null(newdata)) {
            pred <- stats::predict(
                object, predict.all = TRUE, probability = TRUE, ...
            )
        } else {
            pred <- stats::predict(
                object, newdata = newdata, predict.all = TRUE,
                probability = TRUE, ...
            )
        }
        if (type == "prob") {
            # If type is "prob", extract probability.
            pred <- attr(pred, "probabilities")
        }
        return(pred)
    }
)
