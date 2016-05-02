#-------------------------------------------------------------------------------
#'	(Internal) Check an object is formula.
#'
#'	@param x an object.
#'	@return returns TRUE if \emph{x} is formula otherwise returns FALSE.
#-------------------------------------------------------------------------------
#	変数がformulaかを調べる。
#
#	Args:
#		x: 変数。
#	Value:
#		xがformulaならTRUE、違えばFALSE。
#-------------------------------------------------------------------------------
is.formula <- function(x) {
	return(is(x, "formula"))
}

