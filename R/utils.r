#------------------------------------------------------------------------------
#'	(Internal) Is a function S3 generic?
#'
#'	This function \emph{roughly} test a function is S3 generic.
#'
#'	@param fun.name a character string naming the function.
#'
#'	@return
#'		returns TRUE if fun.name is S3 generic function and FALSE otherwise.
#'
#'	@examples
#'	is.s3.generic("plot")
#'	is.s3.generic("glm")
#'	@describeIn is.generic (Internal) function for S3 generic function.
#------------------------------------------------------------------------------
is.s3.generic <- function(fun.name) {
	fun <- match.fun(fun.name)
	fun.text <- as.character(deparse(body(fun)))
	result <- sapply(fun.text, grepl, pattern = "UseMethod\\(\"")
	result <- any(result)
	return(result)
}


#------------------------------------------------------------------------------
#'	(Internal) Is a function S4 generic?
#'
#'	This function \emph{roughly} test a function is S4 generic.
#'
#'	@param fun.name a character string of function name.
#'	@param packae a character string of package name.
#'
#'	@return
#'		returns TRUE if fun.name is S4 generic function and FALSE otherwise.
#'
#'	@examples
#'	is.s4.generic("plot")
#'	is.s4.generic("lmer")
#'	@describeIn is.generic (Internal) function for S4 generic function.
#------------------------------------------------------------------------------
is.s4.generic <- function(fun.name, package = "") {
	search.path <- sprintf("package:%s", package)
	if (search.path %in% search()) {
		return(isGeneric(fun.name, sprintf("package:%s", package)))
	} else {
		return(isGeneric(fun.name))
	}
}


#------------------------------------------------------------------------------
#'	(Internal) Is a function generic?
#'
#'	This function \emph{roughly} test a function is generic.
#'
#'	@param fun.name a character string naming the function.
#'	@param packae a character string of package name.
#'
#'	@return returns TRUE if fun.name is generic function and FALSE otherwise.
#'
#'	@examples
#'	is.generic("plot")
#'	is.generic("glm")
#------------------------------------------------------------------------------
is.generic <- function(fun.name, package = "") {
	return(is.s3.generic(fun.name) | is.s4.generic(fun.name, package))
}


#------------------------------------------------------------------------------
#'	(Internal) match.call() with handling of generic function.
#'
#'	Some generic functions have different arguments for different classes. In
#'	such cases, match.call() function returns wrong results. This function
#'	finds actual generic function called for focal class and matches call with
#'	the function. If call is not a call for non-generic function, matching is
#'	done by match.call() function.
#'
#'	If an object has more than two classes, the first class in the result of
#'	class() with generic function is used.
#'
#'	@param call a call to be matched.
#'	@param envir an environment to evaluate call.
#'	@param package
#'		a character string representing package name of the funciton.
#'
#'	@return matched fully-named call.
#'
#'	@examples
#'		match.generic.call(substitute(hist(1:10)))
#------------------------------------------------------------------------------
match.generic.call <- function(call, envir, package = "") {
	# Non-generic functions.
	fun.name <- get.function(call, "character", envir)
	if (!is.generic(fun.name, package)) {
		fun <- match.fun(fun.name)
		return(match.call(fun, call))
	}
	if (is.s4.generic(fun.name, package)) {
		stop("Algorithm for S4 generic method is not implimented.")
	}
	return(match.generic.call.s3(call, envir))
}


#------------------------------------------------------------------------------
#	match.call considering generic functions.
#
#	Args:
#		matched.call (call):
#			a call to be matched.
#		envir (environment):
#			the environment where calls are evaludated.
#------------------------------------------------------------------------------
match.generic.call.s3 <- function(call, envir) {
	# Match function without considering generic function.
	fun.name <- get.function(call, "character", envir)
	fun <- match.fun(fun.name)
	matched.call <- match.call(fun, call)
	# Find the class name of the argument used for the S3 generic dispatch.
	generic.class.arg <- names(formals(fun))[1]
	generic.classes <- class(eval(matched.call[[generic.class.arg]], envir))
	# If the call does not have the named argument for S3 dispatch,
	# use the first argument of the call.
	if (length(generic.classes) == 1) {
		if (generic.classes == "NULL") {
			generic.classes <- class(eval(matched.call[[2]], envir))
		}
	}
	# Find generic function for each class.
	for (i in generic.classes) {
		fun <- getS3method(fun.name, i, TRUE)
		if (!is.null(fun)) {
			return(match.call(fun, call))
		}
	}
	# If generic function was not found, try default method.
	try(return(match.call(getS3method(fun.name, "default"), call)))
	stop(sprintf("Couldn't find matched generic function of %s.", fun.name))
}


#------------------------------------------------------------------------------
#'	(Internal) Check an object is formula.
#'
#'	@param x an object.
#'	@return returns TRUE if \emph{x} is formula otherwise returns FALSE.
#------------------------------------------------------------------------------
is.formula <- function(x) {
	return(is(x, "formula"))
}


#------------------------------------------------------------------------------
#'	(Internal) Get family from function call.
#'
#'	@param x a call of a function.
#'	@param envir an environment where \code{x} is evaluated.
#'	@return returns family in the call if available else NULL.
#------------------------------------------------------------------------------
extract.family.from.call <- function(x, envir = parent.frame()) {
	f <- x$family
	if (is.null(f)) {
		# If family was not specified, try to get default family.
		fun <- get.function(x, "function", envir)
		f <- formals(fun)$family
	}
	return(f)
}


#------------------------------------------------------------------------------
#'	(Internal) Format and check consistency of family.
#'
#'	@param family
#'		a family function, character of family name, symbol, or family object.
#'	@param type
#'		a character to specify the type of the value this function returns.
#'		If "family", this function returns 'family' object. If "character" is
#'		specified, this function returns a character string denoting the name
#'		of the family.
#'
#'		This internal function is called by \code{link{detect.model.type}} and
#'		\code{\link{predict.glmmML}} functions.
#------------------------------------------------------------------------------
format.family <- function(family, type = c("family", "character")) {
	type <- match.arg(type)
	if (is.character(family)) {
		family <- get(family)
	}
	if (is.symbol(family) | is.language(family)) {
		family <- eval(family)
	}
	if (is.function(family)) {
		family <- family()
	}
	if (is.null(family$family)) {
		print(family)
		stop("`family' not recognized")
	}
	result <- switch(type, family = family, character = family$family)
	return(result)
}


#------------------------------------------------------------------------------
#'	(Internal) Find original name of the function.
#'
#'	@param fun a function
#'	@return
#'		a character string of original function name.
#'		if the function specified as fun is 'original' of it,
#'		this funciton returns name of fun itself.
#'		Note that this function cannot work correctly if a copy of the function
#'		and the original function are in the same environment.
#'
#'	@examples
#'	a <- ls
#'	find.original.name(a)
#'
#'	a <- glm
#'	find.original.name(a)
#'
#'	a <- function() print("test")
#'	find.original.name(a)
#'
#'	# But in the following situation, this can't work correctly.
#'	test <- function() print("test")
#'	a <- test
#'	find.original.name(a)
#------------------------------------------------------------------------------
find.original.name <- function(fun) {
	objects <- ls(envir = environment(fun))
	for (i in objects) {
		if (identical(fun, get(i, envir = environment(fun)))) {
			return(i)
		}
	}
}


#------------------------------------------------------------------------------
#'	(Internal) Get function and function name from call
#'
#'	@param call
#'		a call from which extract function.
#'	@param type
#'		a character vector denoting type of return value.
#'		"function" or "character" can be used. "character" is default.
#'
#'	@return
#'		If type is "function", function object.
#'		If type is "character", character literal of function name.
#'		Note that if the function in the call is copy of other function,
#'		this funciton tries to find original name of it.
#'
#'	@examples
#'		get.function(substitute(glm(Sepal.Length ~ ., data = iris)))
#------------------------------------------------------------------------------
get.function <- function(
	call, type = c("function", "character"), envir = parent.frame()
) {
	# Check arguments
	if (missing(type)) {
		type <- "character"
	}
	type <- match.arg(type)
	# Extract information
	function.name <- as.character(deparse(call[[1]]))
	function.name <- gsub(".*::", "", function.name)
	fun <- get(function.name, envir = envir)
	if (type == "character") {
		function.name <- find.original.name(fun)
		return(function.name)
	} else {
		return(fun)
	}
}


#------------------------------------------------------------------------------
#'	(Internal) Get names of explanatory variables (excluding random factor) from formula.
#'
#'	@param formula a formula object from which variable names are obtained.
#'	@param data a data.frame used to expand . in the formula.
#'	@param specials
#'		special characterss passed to \code{\link[stats]{terms.formula}}.
#'	@param type
#'		a character literal specifying type of explanatory variables to get.
#'
#'	@return
#'		a character vector of names of explanatory variables.
#'		If \emph{type} is "all", returns all explanatory variables in the
#'		formula as is.
#'		If \emph{type} is "base", this function returns all explanatory
#'		variables in their basic form.
#'
#'	@examples
#'		# Getting explanatory variables from formula.
#'		# . in formula is expanded to full form.
#'		data(iris)
#'		f <- Sepal.Length ~ .
#'		x.vars(f, data = iris)
#'
#'		# Getting explanatory variables in their basic form.
#'		f <- Sepal.Length ~ Petal.Length + Petal.Length:Species + I(Sepal.Width^2)
#'		x.vars(f, data = iris, type = "base")
#------------------------------------------------------------------------------
x.names.from.formula <- function(
	formula, data = NULL, specials = NULL, type = c("all", "base")
) {
	# Get all explanatory variables from formula.
	type = match.arg(type)
	t <- terms(formula, data = data, specials = specials)
	vars <- attr(t, "term.labels")
	# Remove random factor.
	vars <- vars[!grepl("\\|", vars)]
	if (type == "all") {
		return(vars)
	}
	# To get basic form of explanatory variables, split interaction.
	vars <- do.call(c, sapply(vars, strsplit, split = ":"))
	# Remove functions, powers, and space.
	powers <- "\\^[1-9]*"
	fun.begin <- "^.*\\("
	fun.end <- "\\)$"
	space <- " "
	remove.chars <- paste(powers, fun.begin, fun.end, space, sep = "|")
	vars <- gsub(remove.chars, "", vars)
	# Remove duplicated and empty names.
	vars <- unique(subset(vars, !vars %in% c("", "1")))
	return(vars)
}
