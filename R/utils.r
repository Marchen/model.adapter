#-------------------------------------------------------------------------------
#	関数が総称関数かをおおざっぱに調べる。
#
#	Args:
#		fun.name: 関数名を表す文字列。
#
#	Value:
#		総称関数ならTRUE、それ以外ならFALSE。
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
#	総称関数もたどってmatch.call()を行う。
#
#	Args:
#		call: 引数をフルネームにするcall。
#		envir: callを評価する環境。
#
#	Value:
#		引数をフルネームにしたcall。
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
#	変数がformulaかを調べる。
#
#	Args:
#		x: 変数。
#	Value:
#		xがformulaならTRUE、違えばFALSE。
#-------------------------------------------------------------------------------
#'	(Internal) Check an object is formula.
#'
#'	@param x an object.
#'	@return returns TRUE if \emph{x} is formula otherwise returns FALSE.
#-------------------------------------------------------------------------------
is.formula <- function(x) {
	return(is(x, "formula"))
}


#-------------------------------------------------------------------------------
#	familyのフォーマットを揃える。
#
#	Args:
#		family: familyオブジェクト、関数、文字列、symbol
#		type:
#			familyならfamilyオブジェクトを、characterならfamily名を表す文字列
#			を返す。
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
#	モデル関数のcall以外を評価する。
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
#'		make.call.or.object(x)
#-------------------------------------------------------------------------------
make.call.or.object <- function(call, env) {
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


#-------------------------------------------------------------------------------
#	formulaから説明変数名を取得する。
#	Args:
#		formula: 説明変数名を取り出す式。
#		data: .を展開するのに使うdata.frame。
#		specials: terms.formulaに渡す特殊文字。
#	Value:
#		説明変数名が格納された文字列ベクトル。
#-------------------------------------------------------------------------------
#'	Get names of explanatory variables from formula.
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
#'		If \emph{type| is "base", this function returns all explanatory 
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
#'
#' @export
#-------------------------------------------------------------------------------
x.names <- function(
	formula, data = NULL, specials = NULL, type = c("all", "base")
) {
	# Get all explanatory variables from formula.
	type = match.arg(type)
	t <- terms(formula, data = data, specials = specials)
	vars <- attr(t, "term.labels")
	if (type == "all") {
		return(vars)
	}
	# Get basic form of explanatory variables.
	# Remove functions and powers
	vars <- do.call(c, sapply(vars, strsplit, split = ":"))
	powers <- "\\^[1-9]*"
	fun.begin <- "^.*\\("
	fun.end <- "\\)$"
	remove.chars <- paste(powers, fun.begin, fun.end, sep = "|")
	vars <- gsub(remove.chars, "", vars)
	vars <- unique(vars)
	return(vars)
}


#-------------------------------------------------------------------------------
#	テスト用パッケージアンローダー。
#-------------------------------------------------------------------------------
#'	(Internal) Package unloader for testing.
#'
#'	@field package.name loaded package name.
#'	@loaded.packages list of loaded package names.
#'
#'	@export
#-------------------------------------------------------------------------------
package.unloader <- setRefClass(
	"package.unloader",
	fields = list(
		package.name = "character",
		loaded.packages = "character"
	),
	methods = list(
		initialize = function(package.name, ignore = options()$defaultPackages) {
			"
			Initialize and load packages.
			\\describe{
				\\item{\\code{package.name}}{package name to load.}
				\\item{\\code{ignore = options()$defaultPackages}}{
					package names which will not be unloaded.
				}
			}
			"
			package.name <<- package.name
			keep <- library(base)
			suppressPackageStartupMessages(
				lib <- library(package.name, character.only = TRUE)
			)
			loaded.packages <<- lib[!lib %in% keep]
			loaded.packages <<- loaded.packages[!loaded.packages %in% ignore]
		},
		unload = function() {
			"
			Unload loaded packages.
			"
			for (package in loaded.packages) {
				detach(
					paste0("package:", package), character.only = TRUE,
					unload = TRUE, force = TRUE
				)
			}
		}
	)
)

