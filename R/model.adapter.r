#------------------------------------------------------------------------------
#' Abstraction layer for model functions/objects
#'
#' This class encapsulates differences in specifications of
#' statistical/machine learning functions and model objects to provide a
#' standardized way to access data and common methods of models.
#' To add support for a modeling function, new generator object of a
#' \code{\link{model.interface}} class must be implimented and methods that
#' cannot work well with the model should be overriden.
#'
#' @field call
#'      a read-only \code{call} object used for initialization of
#'      \code{model.adapter} or a \code{call} object which used for
#'      construction of the model object.
#'      Note that \code{call} in this field is modified to have their full
#'      names by \code{match.call()} function.
#'      Therefore, the \code{call} in this field doesn't need to be identical
#'      to original \code{call} used for initialization of the class.
#'      When the original \code{call} used for creating the model object could
#'      not be recovered from the object, this field becomes \code{NULL}.
#'
#' @field object
#'      a read-only object of the model.
#'      If \code{model.adapter} is initialized by a \code{call} for a model
#'      function, this field returns a object created by evaluating the
#'      \code{call}.
#'
#' @field data
#'      a read-only \code{data.frame} used for modeling.
#'      At least, all columns used for the modeling are stored in this field.
#'      Because some modeling function doesn't keep original \code{data.frame}
#'      used for modeling or \code{call} in resultant object, this field can't
#'      have the same \code{data.frame} used for modeling in such situation.
#'      When all the columns used for the modeling is not available from this
#'      field becomes \code{NULL}.
#'      Also, when the model doesn't store original data and some function
#'      such as `log()` was applied in model construction, the `data.frame`
#'      stored in this field can be different from original values.
#'      At least, this is applicable for `glmer` and `lmer`.
#'
#' @field formula
#'      a read-only \code{formula} object specifying structure of the model.
#'      '.' in the \code{formula} object is expanded so that this field may not
#'      be same as the original \code{formula} specified in the \code{call} for
#'      the model function or the model object in such case.
#'
#' @field family
#'      a read-only character of family name.
#'      If the model does not use family, this field is \code{NULL}.
#'
#' @field link
#'      a function object of link function of the model. If the model does
#'      not have link function, this field has \code{\link[base]{identity}}
#'      function.
#'
#' @field linkinv
#'      a function object of inverse link function of the model. If the model
#'      does not have inverse link function, this field has
#'      \code{\link[base]{identity}} function.
#'
#' @field package.name
#'      a read-only character spcifying package name of the model.
#'
#' @field model.type
#'      a character representing model type. The possible values are
#'      "regression" and "classification".
#'
#' @field x.vars
#'      a data.frame having all explanatory variables.
#'
#' @field y.vars
#'      a data.frame having all response variables.
#'
#' @section Methods:
#'
#'      \strong{\code{model.adapter$new(x, envir = parent.frame(2L), data = NULL, package.name = NULL)}}
#'
#'      Generate a new object.
#'
#'      \subsection{Args}{
#'          \describe{
#'              \item{\code{x}}{
#'                  an object of supported models or a call for a model
#'                  function.
#'              }
#'              \item{\code{envir = parent.frame(2L)}}{
#'                  an environment in which call in x is evaluated.
#'                  Default value is the environment where the object of
#'                  model.adapter was created.
#'              }
#'              \item{\code{data = NULL}}{
#'                  a data.frame used for further manipulations.
#'              }
#'              \item{\code{package.name = NULL}}{
#'                  a character specifying package name of the
#'                  function/object. If not specified, package.name is
#'                  automatically determined from \code{x}. This is mainly
#'                  used to control functions with same name in different
#'                  packages (e.g., \code{gam} in \code{gam} and
#'                  \code{mgcv} packages).
#'              }
#'          }
#'      }
#'
#'      \strong{\code{model.adapter$x.names(specials = NULL, type = c("all", "base"))}}
#'
#'      Access names of explanatory variables.
#'
#'      \subsection{Args}{
#'          \describe{
#'              \item{\code{specials = NULL}}{
#'                  special characters to be passed to \link{terms}.
#'              }
#'              \item{\code{type = c("all", "base")}}{
#'                  if "all", this function returns all explanatory variables
#'                  including interactions, higher order terms, splines, etc.
#'                  If "base" only basic form of the variables are returned.
#'              }
#'          }
#'      }
#'
#'      \subsection{Returns}{
#'          a character vector of names of explanatory variables.
#'      }
#'
#'      \strong{\code{model.adapter$y.names()}}
#'
#'      Access names of response variables.
#'
#'      \strong{\code{model.adapter$predict(newdata = NULL, type = c("response", "link", "prob", "class"), random = ~0, ...)}}
#'
#'      Call predict method.
#'
#'      For regression models, offset term is adjusted if 1) type is not "link",
#'      2) the model having only one offset term, 3) offset was specified as
#'      \code{offset = offset} or \code{formula = y ~ x + offset(offset)}
#'      rather than \code{offset = data$offset} or
#'      \code{formula = y ~ x + offset(data$offset)}, 4) the model is not
#'      \code{glmmML} or \code{ranger}.
#'
#'      Also, it is users responsibility to use offset term with appropreate
#'      scale. For example if the model is \code{glm} with \code{poisson}
#'      family with \code{log} link function, the offset term should be
#'      \code{offset = log(offset)} or
#'      \code{formula = y ~ x + offset(log(offset))}.
#'
#'      \subsection{Args}{
#'          \describe{
#'              \item{\code{newdata = NULL}}{
#'                  a \code{data.frame} containing data used for
#'                  prediction.
#'              }
#'              \item{
#'                  \code{type = c("response", "link", "prob", "class")}
#'              }{
#'                  the type of prediciton. "response" and "link" can be
#'                  used for generalized linear (mixed) model and spcify
#'                  scale of prediction.
#'                  "response" is on the scale of the response variable.
#'                  "link" is on the scale of the link function.
#'                  "prob" and "class" are used for classification models.
#'                  "prob" calculate probability of being each class of
#'                  response variable.
#'                  "class" makes predicted class for each observation.
#'              }
#'              \item{\code{random = ~0}}{
#'                  the random effect to use.
#'                  Tentatively, ~0 means don't use random effects.
#'              }
#'              \item{\code{...}}{
#'                  other variables passed to predict methods.
#'              }
#'          }
#'      }
#'
#'      \subsection{Returns}{
#'          a \code{
#'              \link[=model.adapter.prediction.class]{model.adapter.prediction}
#'          } object.
#'      }
#'
#'      \strong{\code{model.adapter$residuals(type = c("response", "link"))}}
#'
#'      Calculate residuals of the model.
#'
#'      \subsection{Args}{
#'          \describe{
#'              \item{type = c("response", "link")}{
#'                  a character specifying type of residual (i.e., scale of
#'                  residual) to be calculated.
#'              }
#'          }
#'      }
#'
#'      \subsection{Returns}{
#'          a numeric vector of residuals.
#'      }
#'
#' @section Details:
#'
#'      When calling \emph{model.adapter$new} using model call by
#'      \code{do.call}, please specify \emph{quote = TRUE}.
#'      Without it, a call of function in \emph{x} is evaluated and unintended
#'      result may be returned.
#'
#' @export model.adapter
#' @name model.adapter-class
#------------------------------------------------------------------------------
model.adapter <- R6::R6Class(
    "model.adapter",
    private = list(
        # The object or call used for the source of data.
        src = NULL,
        # The environment to evaluate the call.
        envir = NULL,
        # The data.frame specified by user.
        user.data = NULL,
        # The package name specified by user.
        user.package.name = NULL,
        # An instance of model.interface class.
        interface = NULL,
        # The object brought by evaluating the call.
        object.cache = NULL
    )
)


#------------------------------------------------------------------------------
#   Initialize new object.
#------------------------------------------------------------------------------
model.adapter$set(
    "public", "initialize",
    function(
        x, envir = parent.frame(2L), data = NULL, package.name = NULL
    ) {
        private$src <- x
        # Store envir.
        if (!is(envir, "environment")) {
            stop(private$message.for.check.argument(envir, "environment"))
        }
        private$envir <- envir
        # Store data.
        if (!(is(data, "data.frame") | is.null(data))) {
            stop(private$message.for.check.argument(data, "data.frame"))
        }
        private$user.data <- data
        # Store package name.
        if (!(is(package.name, "character") | is.null(package.name))) {
            stop(private$message.for.check.argument(package.name, "character"))
        }
        private$user.package.name <- package.name
        private$init.interface()
    }
)


#------------------------------------------------------------------------------
#   Create error message for parameter checking.
#
#   Args:
#       arg (any):
#           argument to check.
#       expected.class (character):
#           expected class for the argument.
#------------------------------------------------------------------------------
model.adapter$set(
    "private", "message.for.check.argument",
    function(arg, expected.class) {
        arg.name <- as.character(deparse(substitute(arg)))
        message.template <- "'%s' should be a %s. '%s' was specified."
        message <- sprintf(
            sprintf(message.template, arg.name, expected.class, "%s"),
            class(arg)
        )
        return(message)
    }
)


#------------------------------------------------------------------------------
#   Initialize model.interface in interface field.
#
#   If the private$src (i.e., the x argument of model.adapter$new()) is an
#   object containing result of a modeling function, inheritance of the
#   model.interface class is determined by usual S3 polymorphism.
#   On the other hand, if private$src is a call for a modeling function,
#   inheritance of the mmodel.interface class is determined by the name of
#   the function in the call.
#   In such case, this function calls model.adapter.CLASS_NAME()
#   function to initialize a new object.
#------------------------------------------------------------------------------
model.adapter$set(
    "private", "init.interface",
    function() {
        if (is.call(private$src)) {
            fun.name <- get.function(private$src, "character", private$envir)
            code <- sprintf(
                "model.interface.%s(%s)",
                get.class.name(fun.name, self$package.name),
                paste0(deparse(private$src), collapse = "")
            )
            private$interface <- eval(parse(text = code), environment())
        } else {
            private$interface <- model.interface(private$src)
        }
    }
)


#------------------------------------------------------------------------------
#   Retrieve call from the object/call.
#------------------------------------------------------------------------------
model.adapter$set(
    "active", "call",
    function() {
        if (is.call(private$src)) {
            call <-private$src
        } else if (!is.null(private$interface$get.call(private$src))) {
            call <- private$interface$get.call(private$src)
        } else {
            return(NULL)
        }
        return(match.generic.call(call, private$envir, self$package.name))
    }
)


#------------------------------------------------------------------------------
#   Returns original object or object created by evaluating the call.
#------------------------------------------------------------------------------
model.adapter$set(
    "active", "object",
    function() {
        if (!is.call(private$src)) {
            return(private$src)
        }
        if (is.null(private$object.cache)) {
            private$object.cache <- eval(private$src, private$envir)
        }
        return(private$object.cache)
    }
)


#------------------------------------------------------------------------------
#   Retrieve original data from the object.
#------------------------------------------------------------------------------
model.adapter$set(
    "active", "data",
    function() {
        if (!is.null(private$user.data)) {
            return(private$user.data)
        }
        d <- private$interface$get.data(
            private$src, envir = private$envir, self$package.name
        )
        return(d)
    }
)


#------------------------------------------------------------------------------
#   Returns formula of the model.
#------------------------------------------------------------------------------
model.adapter$set(
    "active", "formula",
    function() {
        formula <- private$interface$get.formula(
            private$src, private$envir, self$package.name
        )
        formula <- private$interface$expand.formula(formula, self$data)
        return(formula)
    }
)


#------------------------------------------------------------------------------
#   Returns family of the model.
#   If the model does not support family, this function returns NULL.
#------------------------------------------------------------------------------
model.adapter$set(
    "active", "family",
    function() {
        family.name <- private$interface$get.family(
            private$src, "character", private$envir
        )
        return(family.name)
    }
)

#------------------------------------------------------------------------------
#   Returns link function of the model.
#------------------------------------------------------------------------------
model.adapter$set(
    "active", "link",
    function() {
        return(private$interface$get.link(private$src, private$envir))
    }
)


#------------------------------------------------------------------------------
#   Returns inverse-link function of the model.
#------------------------------------------------------------------------------
model.adapter$set(
    "active", "linkinv",
    function() {
        return(private$interface$get.linkinv(private$src, private$envir))
    }
)


#------------------------------------------------------------------------------
#   Returns package name of the model.
#------------------------------------------------------------------------------
model.adapter$set(
    "active", "package.name",
    function() {
        if (!is.null(private$user.package.name)) {
            return(private$user.package.name)
        } else {
            return(model.adapter:::package.name(private$src, private$envir))
        }
    }
)


#------------------------------------------------------------------------------
#   Returns model type ("regression" or "classification") of the model.
#------------------------------------------------------------------------------
model.adapter$set(
    "active", "model.type",
    function() {
        model.type <- private$interface$get.model.type(
            private$src, private$envir, self$package.name
        )
        return(model.type)
    }
)


#------------------------------------------------------------------------------
#   Returns a data.frame having explanatory variable(s) of the model.
#------------------------------------------------------------------------------
model.adapter$set(
    "active", "x.vars",
    function() {
        return(self$data[self$x.names(type = "base")])
    }
)


#------------------------------------------------------------------------------
#   Returns a data.frame having response variable(s) of the model.
#------------------------------------------------------------------------------
model.adapter$set(
    "active", "y.vars",
    function() {
        return(self$data[self$y.names()])
    }
)


#------------------------------------------------------------------------------
#   Returns a character vector representing names of explanatory variables.
#------------------------------------------------------------------------------
model.adapter$set(
    "public", "x.names",
    function(specials = NULL, type = c("all", "base")) {
        type <- match.arg(type, c("all", "base"))
        return(
            x.names.from.formula(self$formula, self$data, specials, type)
        )
    }
)


#------------------------------------------------------------------------------
#   Returns a character vector representing names of response variables.
#------------------------------------------------------------------------------
model.adapter$set(
    "public", "y.names",
    function() {
        # To handle errors produced by functions like gamm, use "try".
        frame <- suppressWarnings(
        	try(model.frame(self$formula, self$data), silent = TRUE)
        )
        if (class(frame) != "try-error" & is.matrix(frame)) {
            y.names <- colnames(model.response(frame))
        } else {
            y.names <- as.character(self$formula[2])
        }
        # Handle multiresponse models (i.e., model with cbind in response).
        regexp <- "cbind\\((.*)\\)"
        if (grepl(regexp, y.names) ) {
            y.names <- strsplit(gsub(regexp, "\\1", y.names), ",")[[1]]
            y.names <- gsub(" |\t", "", y.names)
        }
        return(y.names)
    }
)


#------------------------------------------------------------------------------
#   Calculate prediction.
#------------------------------------------------------------------------------
model.adapter$set(
    "private", "run.predict",
    function(
        newdata = NULL, type = c("response", "link", "prob", "class"),
        random = ~ 0, ...
    ) {
        pred <- private$interface$predict(
            self$object, newdata = newdata,
            type = private$interface$predict.types[type],
            random = random, ...
        )
        # Adjust offset.
        if (self$model.type == "regression") {
            pred <- private$interface$adjust.offset(
                private$src, private$envir, self$package.name, pred, newdata,
                type
            )
        }
        return(pred)
    }
)


#------------------------------------------------------------------------------
#   Call predict method of the model.
#------------------------------------------------------------------------------
model.adapter$set(
    "public", "predict",
    function(
        newdata = NULL, type = c("response", "link", "prob", "class"),
        random = ~0, ...
    ) {
        # Error check.
        type <- match.arg(type, c("response", "link", "prob", "class"))
        error <- self$model.type == "regression" & type %in% c("prob", "class")
        if (error) {
            stop(
                "'prob' and 'class' types are not compatible ",
                "with regression model."
            )
        }
        pred <- private$run.predict(newdata, type, random, ...)
        # Make model.adapter.prediction object.
        args <- as.list(match.call())[-1]
        pred <- model.adapter.prediction(
            pred, type = type, fixed = newdata[self$x.names(type = "base")],
            interval.type = args$interval, interval.level = args$level,
            logical.response = is.logical(self$y.vars[[1]])
        )
        return(pred)
    }
)


#------------------------------------------------------------------------------
#   Calculate residuals.
#------------------------------------------------------------------------------
model.adapter$set(
    "public", "residuals",
    function(type = c("response", "link")) {
        "
        Return residuals of the model.
        "
        type <- match.arg(type, c("response", "link"))
        # Calculate residual as (response variable) - (predicted value)
        pred <- self$predict(type = type)$fit[, "fit"]
        y <- self$y.vars[[1]]
        fun <- ifelse(type == "link", self$link, identity)
        resid <- fun(y) - pred
        return(resid)
    }
)


#------------------------------------------------------------------------------
#   Check if model is zero inflated.
#------------------------------------------------------------------------------
model.adapter$set(
    "active", "zero_inflated",
    function() {
        return(private$interface$zero_inflated(private$src))
    }
)
