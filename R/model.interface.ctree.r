#------------------------------------------------------------------------------
#'	(Internal) model.interface class for ctree
#'
#'	This reference class contains methods for \code{\link[party]{ctree}} in
#'	\emph{party} package.
#'	Note that because an object of BinaryTree does not keep original call,
#'	get.call() function always returns NULL. Also, when an instance of this
#'	class is made from model object, 'call' field is always call("<undef>").
#'
#'	Following methods are overriden.
#
#'	@include model.interface.default.r
#'	@family model.interface
#------------------------------------------------------------------------------
model.interface.BinaryTree <- setRefClass(
	"model.interface.BinaryTree", contains = "model.interface"
)


#------------------------------------------------------------------------------
model.interface.BinaryTree$methods(
	get.call = function(x) {
		return(NULL)
	}
)


#------------------------------------------------------------------------------
model.interface.BinaryTree$methods(
	get.formula = function(x, envir, package = "") {
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
model.interface.BinaryTree$methods(
	get.data = function(x, envir, package = "", ...) {
		if (is.call(x)) {
		   	return(callSuper(x, envir, package, ...))
		} else {
			# Shared method with cforest.
			interface <- model.interface.RandomForest(x)
			return(interface$get.data(x, envir, package, ...))
		}
	}
)


#------------------------------------------------------------------------------
model.interface.BinaryTree$methods(
	predict.types = function() {
		return(make.predict.types(link = "response", class = "response"))
	}
)


#------------------------------------------------------------------------------
model.interface.BinaryTree$methods(
	predict = function(object, newdata = NULL, type, ...) {
		pred <- stats::predict(object, newdata = newdata, type = type)
		if (type == "prob") {
			pred <- do.call(rbind, pred)
			response <- object@data@get("response")
			colnames(pred) <- levels(response[[colnames(response)]])
		}
		return(pred)
	}
)
