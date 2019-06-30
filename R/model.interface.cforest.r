#------------------------------------------------------------------------------
#'	(Internal) model.interface class for cforest
#'
#'	This reference class contains methods for \code{\link[party]{cforest}} in
#'	\emph{party} package.
#'	Note that because an object of RandomForest does not keep original call,
#'	get.call() function always returns NULL.
#'
#'	@include model.interface.default.r
#'	@name model.interface.RandomForest-class (party)
#------------------------------------------------------------------------------
model.interface.RandomForest.class <- R6::R6Class(
	"model.interface.RandomForest", inherit = model.interface.default.class
)


#------------------------------------------------------------------------------
#'	@method model.interface RandomForest
#'	@export
#'	@describeIn model.interface S3 method for class 'RandomForest'
#------------------------------------------------------------------------------
model.interface.RandomForest <- model.interface.RandomForest.class$new


#------------------------------------------------------------------------------
model.interface.RandomForest.class$set(
	"public", "get.call",
	function(x) {
		# RandomForest class does not have call.
		return(NULL)
	}
)


#------------------------------------------------------------------------------
model.interface.RandomForest.class$set(
	"public", "get.formula",
	function(x, envir, package = "") {
		if (is.object(x)) {
			# Manually construct formula.
			y <- as.character(x@data@formula$response[2])
			x <- as.character(x@data@formula$input[2])
			f <- as.formula(paste(y, x, sep = "~"))
			return(f)
		} else {
			x <- match.call(cforest, x)
			return(eval(x$formula, envir))
		}
	}
)


#------------------------------------------------------------------------------
model.interface.RandomForest.class$set(
	"public", "get.data",
	function(x, envir, package = "", ...) {
		if (is.call(x)){
			return(super$get.data(x, envir, package, ...))
		} else {
			input <- x@data@get("input")
			response <- x@data@get("response")
			d <- cbind(input, response)
		}
	}
)


#------------------------------------------------------------------------------
model.interface.RandomForest.class$set(
	"public", "get.model.type",
	function(x, envir, package = "", ...) {
		response <- x@data@get("response")
		if (is(response, "factor") | is(response, "character")) {
			return("classification")
		} else {
			return("regression")
		}
	}
)


#------------------------------------------------------------------------------
model.interface.RandomForest.class$set(
	"active", "predict.types",
	function() {
		return(make.predict.types(link = "response", class = "response"))
	}
)


#------------------------------------------------------------------------------
model.interface.RandomForest.class$set(
	"public", "predict",
	function(object, newdata = NULL, type, ...) {
		pred <- stats::predict(object, newdata = newdata, type = type)
		if (type == "prob") {
			pred <- do.call(rbind, pred)
			# Remove the name of response variable from the column name.
			f <- self$get.formula(object, envir = parent.frame())
			y.name <- as.character(f[2])
			y.name <- gsub("\\.", "\\\\.", y.name)
			remove.chars <- paste0(y.name, "\\.")
			colnames(pred) <- gsub(remove.chars, "", colnames(pred))
		}
		return(pred)
	}
)
