#-------------------------------------------------------------------------------
#'	(Internal) Get function and function name from call
#'
#'	@param call
#'		a call from which extract function.
#'	@param type
#'		type of return value.
#'		name
#'
#'	@return
#'		If type is "character", function object.
#'		If type is "character", character literal of function name.
#'
#'	@examples
#'		get.function(substitute(glm(Sepal.Length ~ ., data = iris)))
#-------------------------------------------------------------------------------
get.function <- function(call, type = c("function", "character")) {
	# Check arguments
	if (missing(type)) {
		type <- "character"
	}
	type <- match.arg(type)
	# Extract information
	function.name <- as.character(call[[1]])
	if (type == "character") {
		return(function.name)
	} else {
		fun <- match.fun(function.name)
		return(fun)
	}
}

