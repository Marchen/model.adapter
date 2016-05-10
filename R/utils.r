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


#-------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------
#	familyのフォーマットを揃える。
#
#	Args:
#		family: familyオブジェクト、関数、文字列、symbol
#		type:
#			familyならfamilyオブジェクトを、characterならfamily名を表す文字列
#			を返す。
#-------------------------------------------------------------------------------
format.family <- function(family, type = c("family", "character")) {
	type <- match.arg(type)
	if (is.character(family)) {
		family <- get(family)
	}
	if (is.symbol(family)) {
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


#-------------------------------------------------------------------------------
#'	(Internal) Get function and function name from call
#'
#'	@param call
#'		a call from which extract function.
#'	@param type
#'		a character vector denoting type of return value.
#'		"function" or "character" can be used. "character" is default.
#'
#'	@return
#'		If type is "character", function object.
#'		If type is "character", character literal of function name.
#'
#'	@examples
#'		get.function(substitute(glm(Sepal.Length ~ ., data = iris)))
#-------------------------------------------------------------------------------
#	関数、関数名をcallから取得する。
#
#	Args:
#		call: 関数呼び出しが入ったcall。
#		type:
#			結果の種類。"function"なら関数を、"character"なら文字列を返す。
#			初期値は"character"
#
#	Value:
#		"function"なら関数を、"character"なら文字列を返す。
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


#-------------------------------------------------------------------------------
#'	(Internal) Keep call of model functions.
#'
#'	To keep call for model functions but not to keep call for subsetting of 
#'	list, array, etc., this function evaluate original 'x' parameter if it
#'	doesn't have function call for models.
#'
#'	@param call a call object of substituted original 'x' parameter.
#'	@param env  an environment in which call is evaluated.
#'
#'	@return
#'		If original 'x' parameter is call, original call in 'x'.
#'		If original 'x' parameter contains call for a model function,
#'		call for the model function.
#'		If original 'x' parameter is a model object, the model object.
#'
#'	@examples
#'		x <- substitute(glm(Sepal.Length ~ ., data = iris))
#'		keep.model.function.call(x)
#-------------------------------------------------------------------------------
#	モデル関数のcall意外を評価する。
#
#	Args:
#		call:
#			substituteをかけたモデル関数、モデルオブジェクト、
#			モデル呼び出しのcall。
#		env:
#			callを評価する環境。
#	Value:
#		モデルオブジェクトもしくはモデル関数の呼び出しを表すcall。
#-------------------------------------------------------------------------------
keep.model.function.call <- function(call, env = parent.frame()) {
	evaluated.call <- eval(call, env)
	if (is.call(evaluated.call)) {
		# If original object is call, return original object.
		# 元のオブジェクトがcallだったら元のオブジェクトを返す。
		return(evaluated.call)
	}
	if (is.call(call)) {
		# For the case if original object is not call but having function call.
		# 元のオブジェクトがcallではないが関数呼び出しを含むとき。
		fun <- as.character(call)[1]
		if (fun %in% c("$", "@", "[[", "[")) {
			# If the function call is above operators, return original object.
			# 関数呼び出しが上の演算子のとき、元のオブジェクトを返す。
			return(evaluated.call)
		} else {
			# If the function call is not the operators return substituted call.
			# 関数呼び出しが演算子ではないときsubstituteした関数呼び出しを返す。
			return(call)
		}
	} else {
		# If original parameter is object, return original object.
		# 元のオブジェクトがモデルオブジェクトだったら元のオブジェクトを返す。
		return(evaluated.call)
	}
}

