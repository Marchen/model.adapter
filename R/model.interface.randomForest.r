#------------------------------------------------------------------------------
#' (Internal) model.interface class for randomForest
#'
#' This reference class contains methods for
#' \code{\link[randomForest]{randomForest}} in \emph{randomForest} package.
#'
#' @include model.interface.default.r
#' @name model.interface.randomForest-class (randomForest)
#------------------------------------------------------------------------------
model.interface.randomForest.class <- R6::R6Class(
    "model.interface.randomForest", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#' @method model.interface randomForest
#' @export
#' @describeIn model.interface S3 method for class 'randomForest'
#------------------------------------------------------------------------------
model.interface.randomForest <- model.interface.randomForest.class$new


#------------------------------------------------------------------------------
model.interface.randomForest.class$set(
    "active", "predict.types",
    function() {
        return(make.predict.types(link = "response"))
    }
)
