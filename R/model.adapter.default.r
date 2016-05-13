#-------------------------------------------------------------------------------
#'	Initialize a model.adapter object.
#'
#'	This function makes an object of a derived class of \emph{model.adapter} 
#'	class that abstracts differences in specifications of supported modeling 
#'	functions.
#'
#'	@param x
#'		an object of supported models or a call for a model function.
#'	@return
#'		an object of derived class of \code{\link{model.adapter-class}}.
#'	@section For developpers:
#'		If x is an object containing result of a statistical models, 
#'		inheritance of the class is determined by usual S3 polymorphism. 
#'		On the other hand, if x is a call for a modeling function, inheritance 
#'		of the class is determined by function name in the function call. 
#'		In such case, this function call model.adapter.CLASS_NAME() to 
#'		initialize object.
#'		
#'	@section Adding support for new function:
#'		To be continued...
#-------------------------------------------------------------------------------
#	モデルの違いを吸収するアダプタークラスのオブジェクトを作る関数。
#
#	Args:
#		x: モデルオブジェクト、もしくはモデル関数の呼び出し。
#
#	Value:
#		model.adapterクラスを継承したクラスのオブジェクト。
#
#-------------------------------------------------------------------------------
model.adapter <- function(x, env = parent.frame(1L)) {
	original.call <- make.call.or.object(substitute(x), env)
	if (is.call(original.call)) {
		fun.name <- get.function(original.call, "character")
		code <- sprintf(
			"model.adapter.%s(%s, env)", get.class.name(fun.name),
			paste0(deparse(original.call), collapse = "")
		)
		object <- eval(parse(text = code), environment())
		return(object)
	}
	UseMethod("model.adapter")
}


#-------------------------------------------------------------------------------
#'	Abstraction layer for model functions/objects.
#'
#'	This class encapsulates differences in specifications of statistical/machine
#'	learning functions and model objects to provide standard way to access 
#'	data and common methods of models. To add support for a new modeling 
#'	function, new generator object of a reference class inheriting this class 
#'	must be implimented and methods that cannot work well with the model should
#'	be overriden.
#'
#'	@field src
#'		a read-only list of following information used for initialization.
#'		\emph{call}: 
#'			a call for modeling function used for initialization of the class.
#'		\emph{object}:
#'			an object of a model used for initialization of the class.
#'		When an object of model.adapter is initialized using call, src$object 
#'		field is NULL. When an object of model.adapter is initialized using 
#'		object, src$call field is NULL.
#'
#'	@field call
#'		a read-only call object used for initialization of model.adapter or a 
#'		call object which used for construction of the model object.
#'		Note call in this field is set in their full names by 
#'		match.call() function. Therefore, it doesn't need to be identical to
#'		original call used for initialization of the class. To check an object
#'		having valid call, use \code{has.call()} method.
#'
#'	@field env
#'		an environment in which call of a model function is evaluated.
#'		By default, this field is set to an environment where 
#'		\code{\link{model.adapter}} function is called.
#'
#'	@field family
#'		a read-only character of family name. If a model which does not use
#'		family, this field is character(0).
#'
#'	@field formula
#'		a formula object specifying structure of the model. '.' in this
#'		formula object is expanded so that this field does not need to be
#'		same as original formula specified in the call for model function or 
#'		the model object.
#'
#'	@field data
#'		a data.frame used for modeling. At least, all columns used for the 
#'		modeling are stored in this field. Because some modeling function 
#'		doesn't keep original \emph{data.frame} used for modeling or 
#'		\emph{call} in resultant object, this field can't have the same 
#'		data.frame used for modeling in such case. When all the columns used
#'		for the modeling is not available from this field is set to a 
#'		data.frame with 0 columns x 0 rows. To test this field have valid
#'		data.frame use \code{has.data()} method.
#'
#'	@export
#-------------------------------------------------------------------------------
#	モデリング関数の違いを吸収するReference Class、model.adapterクラスの
#	ジェネレーターオブジェクト。
#	この基底クラスを継承して、様々なモデルに対応させる。
#
#		Fields:
#			src: 初期化に使った情報を格納。
#				src$call: 初期化に使ったcall。
#				src$object: 初期化に使ったオブジェクト。
#				callで初期化された場合、src$objectはNULL。
#				objectで初期化された場合、src$callはNULL。
#			call:
#				モデルの呼び出し式。
#			env:
#				callを評価する環境。
#			family:
#				モデルのファミリーを表す文字列。
#				familyがないモデルの場合はcharacter(0)。
#			formula:
#				モデル式。
#			data:
#				モデル作成に使われたデータ。
#				モデルオブジェクトはデータを保持していないことがあるので、
#				元のデータフレームが取得できないことがある。
#		Methods:
#			以下を参照。
#-------------------------------------------------------------------------------
model.adapter.default <- setRefClass(
	"model.adapter",
	fields = list(
		src = "list",
		call = "call",
		env = "environment",
		family = "character",
		formula = "formula",
		data = "data.frame"
	)
)


#-------------------------------------------------------------------------------
#	コンストラクタ
#	Args:
#		x: モデルオブジェクトもしくはモデルの呼び出しを表すcall。
#		envir: callを評価する環境。
#		...: 他のメソッドに渡される引数。
#		caller:
#			サブクラスからこのメソッドを呼ぶときには"subclass"を指定する。
#-------------------------------------------------------------------------------
model.adapter.default$methods(
	initialize = function(
		x, envir = parent.frame(4L), ..., caller = "default"
	) {
		"
		Initialize an object of the class using model object or call for a
		model function.
		\\describe{
			\\item{\\code{x}}{model object or function call.}
			\\item{\\code{envir = parent.frame(4L)}}{
				envir an environment in which call in x is evaluated.
			}
			\\item{\\code{...}}{arguments to be passed to methods.}
			\\item{\\code{caller = \"default\"}}{
				if this method is called from an initialize method of subclass,
				this should be set to \"subclass\".
			}
		}
		"
		# Initialize src field. / srcフィールドの初期化。
		if (caller != "subclass"){
			x <- substitute(x)
		}
		x <- make.call.or.object(x, envir)
		if (is.call(x)) {
			src$call <<- x
		} else {
			src$object <<- x
		}
		# Initialize call field. / callフィールドの初期化。
		if (is.call(x)) {
			call <<- match.generic.call(x)
		} else {
			if (!is.null(.self$get.call(x))) {
				call <<- .self$get.call(x)
			}
		}
		# Initialize env field. / envフィールドの初期化。
		env <<- envir
		# Initialize family field. / familyフィールドの初期化。
		family.name <- .self$get.family(x)
		if (!is.null(family.name)) {
			family <<- format.family(family.name, "character")
		}
		# Initialize formula field. / formulaフィールドの初期化。
		if (.self$has.call()) {
			formula <<- .self$get.formula(call, .self$env)
		} else {
			formula <<- .self$get.formula(src$object, .self$env)
		}
		# Initialize data field. / dataフィールドの初期化。
		d <- .self$get.data(x, envir = .self$env)
		if (!is.null(d)){
			data <<- .self$get.data(x)
		}
	}
)


#-------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#	Args:
#		x: 関数呼び出しのcall、もしくはモデルオブジェクト。
#-------------------------------------------------------------------------------
model.adapter.default$methods(
	get.family = function(x) {
		"
		Get family from call or model object. 
		If family was not specified, return NULL.
		\\describe{
			\\item{\\code{x}}{call or model object.}
		}
		"
		if (is.call(x)) {
			return(x$family)
		} else {
			if (isS4(x)) {
				if ("family" %in% slotNames(x)) {
					return(src$object@family)
				} else {
					return(NULL)
				}
			} else {
				return(x$family)
			}
		}
	}
)


#-------------------------------------------------------------------------------
#	モデルのcallを取得する。
#	Args:
#		x: はモデルオブジェクト。
#	Value:
#		オブジェクトを作ったcall。
#-------------------------------------------------------------------------------
model.adapter.default$methods(
	get.call = function(x) {
		"
		This method returns call by which the object is made. If call is not 
		available, this returns NULL. To distinguish the return value of NULL 
		is intended action or not, inherited classes are encouraged to 
		inherit this method to explicitly return NULL if x does not have call.
		\\describe{\\item{\\code{x}}{a model object.}}
		"
		if (isS4(x)) {
			result <- x@call
		} else {
			result <- x$call
			if (is.null(result)) {
				warning("get.call() implicitly returns NULL. Is it intended?")
			}
		}
		return(result)
	}
)


#-------------------------------------------------------------------------------
#	callに有効なcallが入っているかを確認する。
#-------------------------------------------------------------------------------
model.adapter.default$methods(
	has.call = function() {
		"
		Check call field has valid call.
		"
		if (identical(call, call("<undef>"))) {
			return(FALSE)
		} else {
			return(TRUE)
		}
	}
)


#-------------------------------------------------------------------------------
#	モデルのformulaを取得する。
#	Args:
#		x: 関数呼び出しのcall、もしくはモデルオブジェクト。
#		envir: xに入ったcallを評価する環境。
#-------------------------------------------------------------------------------
model.adapter.default$methods(
	get.formula = function(x, envir = parent.frame()) {
		"
		Extract formula from model object/call.
		\\describe{
			\\item{\\code{x}}{
				a model object/call from which formula is extracted.
			}
			\\item{\\code{envir = parent.frame()}}{
				an environment in which call in x is evaluated.
			}
		}
		"
		if (is.object(x)) {
			if (isS4(x)) {
				return(eval(x@call$formula, envir))
			} else {
				return(eval(x$call$formula, envir))
			}
		} else {
			if (!is.null(x$formula)) {
				return(eval(x$formula, envir))
			} else {
				args <- lapply(as.list(x), eval, envir = envir)
				f <- args[sapply(args, is.formula)][[1]]
				return(f)
			}
		}
	}
)


#-------------------------------------------------------------------------------
#	モデルのdataを取得する。
#	Args:
#		x: 関数呼び出しのcall、もしくはモデルオブジェクト。
#		envir: xに入ったcallを評価する環境。
#-------------------------------------------------------------------------------
model.adapter.default$methods(
	get.data = function(x, envir = parent.frame()) {
		"
		Get data used for modeling.
		\\describe{
			\\item{\\code{x}}{
				a model object/call from which data is extracted.
			}
			\\item{\\code{envir = parent.frame()}}{
				an environment in which call is evaluated.
			}
		}
		"
		if (is.object(x)) {
			if (isS4(x)) {
				return(x@data)
			} else {
				return(x$data)
			}
		} else {
			eval(x$data, envir)
		}
	}
)


#-------------------------------------------------------------------------------
#	formulaの.を展開する。
#-------------------------------------------------------------------------------
model.adapter.default$methods(
	expand.formula = function(f, d, specials = NULL) {
		"
		Expand . in formula.
		\\describe{
			\\item{\\code{f}}{a formula to expand.}
			\\item{\\code{d}}{a data.frame used to expand . in formula.}
			\\item{\\code{specials}}{
				special characterss passed to 
				\code{\link[stats]{terms.formula}}.
			}
		}
		"
		result <- terms(f, data = d, specials = specials)
		attributes(result) <- NULL
		result <- as.formula(result)
		return(result)
	}
)


for (field.name in names(model.adapter.default$fields)) {
	model.adapter.default$lock(n)
}
rm(field.name)
