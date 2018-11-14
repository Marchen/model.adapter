#------------------------------------------------------------------------------
#'	model.interface class for cforest
#'
#'	This reference class contains methods for \code{\link[party]{cforest}} in
#'	\emph{party} package.
#'	Note that because an object of RandomForest does not keep original call,
#'	get.call() function always returns NULL. Also, when an instance of this
#'	class is made from model object, 'call' field is always call("<undef>").
#'
#'	Following methods are overriden.
#
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.RandomForest
#'	@exportClass model.interface.RandomForest
#------------------------------------------------------------------------------
model.interface.RandomForest <- setRefClass(
	"model.interface.RandomForest", contains = "model.interface"
)


#------------------------------------------------------------------------------
model.interface.RandomForest$methods(
	get.call = function(x) {
		return(NULL)
	}
)

#------------------------------------------------------------------------------
model.interface.RandomForest$methods(
	get.formula = function(x, envir, package = "") {
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
model.interface.RandomForest$methods(
	get.data = function(x, envir, package = "", ...) {
		if (is.call(x)){
			return(callSuper(x, envir, package, ...))
		} else {
			input <- x@data@get("input")
			response <- x@data@get("response")
			d <- cbind(input, response)
		}
	}
)


#------------------------------------------------------------------------------
model.interface.RandomForest$methods(
	predict.types = function() {
		return(make.predict.types(link = "response", class = "response"))
	}
)


#------------------------------------------------------------------------------
model.interface.RandomForest$methods(
	predict = function(object, newdata = NULL, type, ...) {
		pred <- stats::predict(object, newdata = newdata, type = type)
		if (type == "prob") {
			pred <- do.call(rbind, pred)
			# Remove the name of response variable from the column name.
			f <- .self$get.formula(object, envir = parent.frame())
			y.name <- as.character(f[2])
			y.name <- gsub("\\.", "\\\\.", y.name)
			remove.chars <- paste0(y.name, "\\.")
			colnames(pred) <- gsub(remove.chars, "", colnames(pred))
		}
		return(pred)
	}
)
