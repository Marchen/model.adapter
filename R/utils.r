#-------------------------------------------------------------------------------
#'	(Internal) Is a function generic?
#'
#'	This function emph{roughly} test a function is generic.
#'
#'	@param fun.name a character string naming the function.
#'
#'	@return returns TRUE if fun is generic function and FALSE otherwise.
#'
#'	@examples
#'	is.generic(plot)
#'	is.generic(glm)
#-------------------------------------------------------------------------------
#	関数が総称関数かをおおざっぱに調べる。
#
#	Args:
#		fun.name: 関数名を表す文字列。
#
#	Value:
#		総称関数ならTRUE、それ以外ならFALSE。
#-------------------------------------------------------------------------------
is.generic <- function(fun.name) {
	is.generic.s3 <- function(fun.name) {
		fun <- match.fun(fun.name)
		fun.text <- as.character(deparse(body(fun)))
		result <- sapply(fun.text, grepl, pattern = "UseMethod\\(\"")
		result <- any(result)
		return(result)
	}
	return(isGeneric(as.character(fun.name)) | is.generic.s3(fun.name))
}


#-------------------------------------------------------------------------------
#'	(Internal) match.call() with handling of generic function.
#'
#'	Some generic functions have different arguments for different classes. In 
#'	such cases, match.call() function returns wrong results. This function 
#'	finds actual generic function called for focal class and matches call with
#'	the function. If call is not a call for non-generic function, matching is
#'	done by match.call() function.
#'
#'	@param call a call to be matched.
#'	@param envir an environment to evaluate call.
#'
#'	@return matched call.
#'
#'	@examples
#'		match.generic.call(substitute(hist(1:10)))
#-------------------------------------------------------------------------------
#	総称関数もたどってmatch.call()を行う。
#
#	Args:
#		call: 引数をフルネームにするcall。
#		envir: callを評価する環境。
#
#	Value:
#		引数をフルネームにしたcall。
#-------------------------------------------------------------------------------
match.generic.call <- function(call, envir = parent.frame(2L)) {
	fun.name <- get.function(call)
	fun <- match.fun(fun.name)
	matched.call <- match.call(fun, call)
	if (!is.generic(fun.name)) {
		# Non-generic functions.
	   	return(matched.call)
	}
	# Find class of generic function / 総称関数の分岐に使われるクラスを取得。
	generic.class.name <- names(formals(fun))[1]
	generic.class <- class(eval(matched.call[[generic.class.name]], envir))
	if (isGeneric(fun.name)) {
		#S4 functions.
		stop("Not implimented.")
	} else {
	   	#S3 functions.
	   	fun <- getS3method(fun.name, generic.class, TRUE)
	   	if (is.null(fun)) {
	   		fun <- getS3method(fun.name, "default")
	   	}
		return(match.call(fun, call))
	}
}


