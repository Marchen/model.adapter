#------------------------------------------------------------------------------
#' (Internal) model.interface class for averaging
#'
#' This reference class contains methods for \emph{averaging} class in
#' \emph{MuMIn} package.
#' Current support for the \emph{averaging} is limited and may not work for
#' all supported models by \emph{MuMIn} package.
#' Also because of it nature, using \emph{averaging} with \emph{model.adapter}
#' have several limitations.
#'
#' \describe{
#'      \item{\code{get.formula}} {
#'          because nature of \emph{\code{averaging}} object,
#'          this class constructs formula from all variable names in
#'          \emph{\code{sw}} field of the object.
#'      }
#'      \item{\code{get.familiy}, \code{get.data}} {
#'          family and data are obtained from the first model of the
#'          \emph{\code{model.calls}} or \emph{\code{modelList}} attributes
#'          depending on the object.
#'      }
#'      \item{\code{adjust.offset}} {
#'          not implemented.
#'      }
#' }
#'
#' @include model.interface.default.r
#' @name model.interface.averaging-class
#------------------------------------------------------------------------------
model.interface.averaging.class <- R6::R6Class(
    "model.interface.averaging", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#' @method model.interface averaging
#' @export
#' @describeIn model.interface S3 method for class 'averaging'
#------------------------------------------------------------------------------
model.interface.averaging <- model.interface.averaging.class$new


#------------------------------------------------------------------------------
model.interface.averaging.class$set(
    "active", "predict.types",
    function() {
        return(make.predict.types(prob = "response", class = "response"))
    }
)

#------------------------------------------------------------------------------
model.interface.averaging.class$set(
    "public", "get.formula",
    function(x, envir, package = "") {
        y.name <- as.character(formula(private$get.object(x))[[2]])
        x.names <- names(x$sw)
        f <- sprintf("%s ~ %s", y.name, paste(x.names, collapse = " + "))
        return(as.formula(f, env = envir))
    }
)

#------------------------------------------------------------------------------
model.interface.averaging.class$set(
    "private", "get.object",
    function(x) {
        object <- attr(x, "model.calls")[[1]]
        if (!is.null(object)) {
            return(object)
        }
        return(attr(x, "modelList")[[1]])
    }
)

#------------------------------------------------------------------------------
model.interface.averaging.class$set(
    "public", "get.family",
    function(x, type = c("character", "family"), envir) {
        object <- private$get.object(x)
        return(model.interface(object)$get.family(object, type, envir))
    }
)

#------------------------------------------------------------------------------
model.interface.averaging.class$set(
    "public", "get.data",
    function(x, envir, package = "", ...) {
        object <- private$get.object(x)
        return(
            model.interface(object)$get.data(object, envir, package, ...)
        )
    }
)

# #------------------------------------------------------------------------------
# model.interface.averaging.class$set(
# "public", "predict",
# function(object, newdata, type, random, ...) {
#     if (is.null(newdata)) {
#         pred <- stats::predict(object, type = type, ...)
#     } else {
#         models <- attr(object, "modelList")
#         if (is.null(models)) {
#             stop("'averaging' object should have 'modelList' attribute")
#         }
#         pred_list <- lapply(
#             models, stats::predict, newdata = newdata, type = type, ...
#         )
#         pred <- colSums(do.call(rbind, pred_list) * object$msTable$weight)
#     }
#     return(pred)
# }
# )

#------------------------------------------------------------------------------
model.interface.averaging.class$set(
    "public", "get.offset.names",
    function(x, envir, package = "") {
        object <- private$get.object(x)
        interface <- model.interface(object)
        return(interface$get.offset.names(object, envir, package))
    }
)
