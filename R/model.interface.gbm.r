#------------------------------------------------------------------------------
#'	(Internal) model.interface class for gbm
#'
#'	This reference class contains methods for \code{\link[gbm]{gbm}} in
#'	\emph{gbm} package.
#'
#'	@include model.interface.default.r
#'	@family model.interface classes
#'	@name model.interface.gbm-class
#------------------------------------------------------------------------------
NULL

model.interface.gbm.class <- R6::R6Class(
	"model.interface.gbm", inherit = model.interface.default.class
)

model.interface.gbm <- model.interface.gbm.class$new


#------------------------------------------------------------------------------
model.interface.gbm.class$set(
	"public", "get.data",
	function(x, envir, package = "") {
		# Extract data from call using default method.
		# If default method couldn't get data, try extracting data in the call
		# of the object.
		if (is.call(x)) {
			d <- super$get.data(x, envir)
		} else {
			model.call <- match.generic.call(self$get.call(x), envir, package)
			d <- eval(model.call$data)
		}
		# If still couldn't get data, use data in the object and make a
		# a data.frame manually.
		if (is.null(d)){
			warning("Making data.frame using data field in gbm object.")
			y.var <- x$data$y
			colnames(y.var) <- x$response.name
			x.vars <- x$data$x
			colnames(x.vars) <- x$var.names
			d <- as.data.frame(cbind(y.var, x.var))
			rownames(d) <- NULL
		}
		return(d)
	}
)


#------------------------------------------------------------------------------
model.interface.gbm.class$set(
	"active", "predict.types",
	function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)


#------------------------------------------------------------------------------
model.interface.gbm.class$set(
	"public", "predict",
	function(object, newdata, type, random, ...) {
		pred <- super$predict(object, newdata, type, random, ...)
		if (is.array(pred)) {
			pred <- as.matrix(pred[, , 1])
		}
		# If the prediction type is "class", convert the result to class label.
		if (names(type) == "class") {
			pred <- colnames(pred)[apply(pred, 1, which.max)]
		}
		return(pred)
	}
)


#------------------------------------------------------------------------------
model.interface.gbm.class$set(
	"public", "get.model.type",
	function(x, envir, package = "", ...) {
		if (is.call(x)) {
			distribution <- x$distribution
			if (is.null(distribution)) {
				y.name <- as.character(self$get.formula(x, envir, package)[2])
				data <- self$get.data(x, envir, package)
				distribution <- gbm::guessDist(data[[y.name]])$name
			}
		} else {
			distribution <- x$distribution$name
		}
		classification.families <- c(
			"bernoulli", "huberized", "multinomial", "adaboost"
		)
		if (distribution %in% classification.families){
			return("classification")
		} else {
			return("regression")
		}
	}
)
