

model.interface <- function(x = NULL) {
	UseMethod("model.interface")
}


model.interface.default <- setRefClass(
	"model.interface",
	methods = list(initialize = function(x) { })
)


#-------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#	Args:
#		x: 関数呼び出しのcall、もしくはモデルオブジェクト。
#-------------------------------------------------------------------------------
model.interface.default$methods(
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
model.interface.default$methods(
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
#	モデルのdataを取得する。
#	Args:
#		x: 関数呼び出しのcall、もしくはモデルオブジェクト。
#		envir: xに入ったcallを評価する環境。
#-------------------------------------------------------------------------------
model.interface.default$methods(
	get.data = function(x, envir = parent.frame()) {
		"
		Get a data.frame containing the data used for modeling.
		If data is not available this method returns empty data.frame made by
		data.frame().
		\\describe{
			\\item{\\code{x}}{
				a model object/call from which data is extracted.
			}
			\\item{\\code{envir = parent.frame()}}{
				an environment in which call is evaluated.
			}
		}
		"
		if (is.call(x)) {
			d <- eval(x$data, envir)
		} else {
			if (isS4(x)) {
				d <- x@data
			} else {
				d <- x$data
			}
			if (is.null(d)) {
				# When couldn't retrieve data from object, get it from call.
				# オブジェクトからdataを取得できなかったらcallから取得を試みる。
				cl <- match.generic.call(.self$get.call(x))
				d <- eval(cl$data, envir)
			}
		}
		return(d)
	}
)


#-------------------------------------------------------------------------------
#	モデルのformulaを取得する。
#	Args:
#		x: 関数呼び出しのcall、もしくはモデルオブジェクト。
#		envir: xに入ったcallを評価する環境。
#-------------------------------------------------------------------------------
model.interface.default$methods(
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
#	formulaの.を展開する。
#-------------------------------------------------------------------------------
model.interface.default$methods(
	expand.formula = function(f, d, specials = NULL, package.name = NULL) {
		"
		Expand . in formula.
		\\describe{
			\\item{\\code{f}}{a formula to expand.}
			\\item{\\code{d}}{a data.frame used to expand . in formula.}
			\\item{\\code{specials = NULL}}{
				special characterss passed to 
				\\code{\\link[stats]{terms.formula}}.
			}
			\\item{\\code{package.name = NULL}}{
				a character literal of package name having the model function.
			}
		}
		"
		result <- terms(f, data = d, specials = specials)
		attributes(result) <- NULL
		result <- as.formula(result)
		return(result)
	}
)


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
#'	Initialize a model.adapter object.
#'
#'	This function makes an object of a derived class of \emph{model.adapter} 
#'	class that abstracts differences in specifications of supported modeling 
#'	functions.
#'
#'	@param x
#'		an object of supported models or a call for a model function.
#'	@param env
#'		an environment where function call in 'x' is evaluated.
#'	@param ...
#'		other arguments passed to methods.
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
#'	@family model.adapter
#'	@export
#-------------------------------------------------------------------------------



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
#'	@export model.adapter
#-------------------------------------------------------------------------------
model.adapter <- setRefClass(
	"model.adapter",
	fields = list(
		src = "list",
		call = "call",
		env = "environment",
		family = "character",
		formula = "formula",
		data = "data.frame",
		interface = "model.interface"
	)
)


#-------------------------------------------------------------------------------
#	コンストラクタ
#	Args:
#		x: モデルオブジェクトもしくはモデルの呼び出しを表すcall。
#		envir: callを評価する環境。
#		data: 
#		...: 他のメソッドに渡される引数。
#-------------------------------------------------------------------------------
model.adapter$methods(
	initialize = function(x, envir = parent.frame(4L), data = NULL, ...) {
		"
		Initialize an object of the class using model object or call for a
		model function.
		\\describe{
			\\item{\\code{x}}{model object or function call.}
			\\item{\\code{envir = parent.frame(4L)}}{
				envir an environment in which call in x is evaluated.
			}
			\\item{\\code{data}}{data.frame used for further manipulations.}
			\\item{\\code{...}}{arguments to be passed to methods.}
			}
		}
		"
		# Initialize interface field. / interfaceフィールドの初期化
		seed <- make.call.or.object(substitute(x), envir)
		if (is.call(seed)) {
			fun.name <- get.function(seed, "character")
			code <- sprintf(
				"model.interface.%s(%s)", get.class.name(fun.name),
				paste0(deparse(seed), collapse = "")
			)
			interface <<- eval(parse(text = code), environment())
		} else {
			interface <<- model.interface(x)
		}
		# Initialize src field. / srcフィールドの初期化。
		if (is.call(seed)) {
			src$call <<- seed
		} else {
			src$object <<- seed
		}
		# Initialize call field. / callフィールドの初期化。
		if (is.call(seed)) {
			call <<- match.generic.call(seed)
		} else {
			if (!is.null(interface$get.call(seed))) {
				call <<- interface$get.call(x)
			}
		}
		# Initialize env field. / envフィールドの初期化。
		env <<- envir
		# Initialize family field. / familyフィールドの初期化。
		family.name <- interface$get.family(x)
		if (!is.null(family.name)) {
			family <<- format.family(family.name, "character")
		}
		# Initialize data field. / dataフィールドの初期化。
		if (is.null(data)) {
			d <- interface$get.data(x, envir = .self$env)
			if (!is.null(d)) {
				data <<- interface$get.data(x)
			}
		} else {
			data <<- data
		}
		# Initialize formula field. / formulaフィールドの初期化。
		if (.self$has.call()) {
			formula <<- interface$get.formula(call, .self$env)
		} else {
			formula <<- interface$get.formula(src$object, .self$env)
		}
		formula <<- interface$expand.formula(formula, .self$data)
	}
)



#for (field.name in names(model.adapter.default$fields)) {
	#model.adapter.default$lock(n)
#}
#rm(field.name)



#-------------------------------------------------------------------------------
#	callに有効なcallが入っているかを確認する。
#-------------------------------------------------------------------------------
model.adapter$methods(
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
#	dataフィールドにデータがあるかを確認。
#-------------------------------------------------------------------------------
model.adapter$methods(
	has.data = function() {
		"
		Test data field has valid data.frame.
		Returns TRUE if valid and returns FALSE if not.
		"
		result <- !(nrow(data) == 0 & ncol(data) == 0)
		return(result)
	}
)
