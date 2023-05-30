#------------------------------------------------------------------------------
#' (Internal) model.interface class for rpart
#'
#' This reference class contains methods for \code{\link[rpart]{rpart}} in
#' \emph{rpart} package.
#'
#' @include model.interface.default.r
#' @name model.interface.rpart-class (rpart)
#------------------------------------------------------------------------------
model.interface.rpart.class <- R6::R6Class(
    "model.interface.rpart", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#' @method model.interface rpart
#' @export
#' @describeIn model.interface S3 method for class 'rpart'
#------------------------------------------------------------------------------
model.interface.rpart <- model.interface.rpart.class$new


#------------------------------------------------------------------------------
model.interface.rpart.class$set(
    "active", "predict.types",
    function() {
        return(make.predict.types(response = "vector", link = "matrix"))
    }
)
