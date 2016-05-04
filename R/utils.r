#-------------------------------------------------------------------------------
#'	Is a function generic?
#'
#'	This function emph{roughly} test a function is generic.
#'
#'	@param fun.name a character string naming the function.
#'
#'	@return returns TRUE if fun is generic function and FALSE otherwise.
#'	@export
#'
#'	@examples
#'	is.generic(plot)
#'	is.generic(glm)
#-------------------------------------------------------------------------------
#	関数が総称関数かをおおざっぱに調べる。
#
#	Args:
#		fun.name: 関数名を表す文字列。
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


