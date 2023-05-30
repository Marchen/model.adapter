#------------------------------------------------------------------------------
#' (Internal) model.interface class for ctree
#'
#' This reference class contains methods for \code{\link[party]{ctree}} in
#' \emph{party} package.
#' Note that because an object of BinaryTree does not keep original call,
#' get.call() function always returns NULL.
#
#' @include model.interface.default.r
#' @include model.interface.cforest.r
#' @name model.interface.BinaryTree-class (party)
#------------------------------------------------------------------------------
model.interface.BinaryTree.class <- R6::R6Class(
    "model.interface.BinaryTree", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#' @method model.interface BinaryTree
#' @export
#' @describeIn model.interface S3 method for class 'BinaryTree'
#------------------------------------------------------------------------------
model.interface.BinaryTree <- model.interface.BinaryTree.class$new


#------------------------------------------------------------------------------
model.interface.BinaryTree.class$set(
    "public", "get.call",
    function(x) {
        # BinaryTree class doesn't have call.
        return(NULL)
    }
)


#------------------------------------------------------------------------------
model.interface.BinaryTree.class$set(
    "public", "get.formula",
    function(x, envir, package = "") {
        if (is.call(x)) {
            x <- match.call(ctree, x)
            return(eval(x$formula, envir))
        } else {
            # Shared method with cforest.
            interface <- model.interface.RandomForest(x)
            return(interface$get.formula(x, envir, package))
        }
    }
)


#------------------------------------------------------------------------------
model.interface.BinaryTree.class$set(
    "public", "get.data",
    function(x, envir, package = "", ...) {
        if (is.call(x)) {
               return(super$get.data(x, envir, package, ...))
        } else {
            # Shared method with cforest.
            interface <- model.interface.RandomForest(x)
            return(interface$get.data(x, envir, package, ...))
        }
    }
)


#------------------------------------------------------------------------------
model.interface.BinaryTree.class$set(
    "public", "get.model.type",
    function(x, envir, package = "", ...) {
        # Shared method with cforest.
        interface <- model.interface.RandomForest()
        return(interface$get.model.type(x, envir, package, ...))
    }
)


#------------------------------------------------------------------------------
model.interface.BinaryTree.class$set(
    "active", "predict.types",
    function() {
        return(make.predict.types(link = "response", class = "response"))
    }
)


#------------------------------------------------------------------------------
model.interface.BinaryTree.class$set(
    "public", "predict",
    function(object, newdata = NULL, type, ...) {
        pred <- stats::predict(object, newdata = newdata, type = type)
        if (type == "prob") {
            pred <- do.call(rbind, pred)
            response <- object@data@get("response")
            colnames(pred) <- levels(response[[colnames(response)]])
        }
        return(pred)
    }
)


#------------------------------------------------------------------------------
model.interface.BinaryTree.class$set(
    "public", "adjust.offset",
    function(x, envir, package, pred, ...) {
        return(pred)
    }
)
