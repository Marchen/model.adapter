#-------------------------------------------------------------------------------
#'	Make a model.adapter object.
#'
#'	This function makes an object of a derived class of \emph{model.adapter} 
#'	class that abstract differences in specifications of supported modeling 
#'	functions.
#'
#'	@param x
#'		an object of supported models or a call for a model function.
#'	@return
#'		an object of derived class of \code{\link{model.adapter-class}}.
#'	@section For developpers
#'		If x is an object containing result of statistical models, inheritance
#'		of model.adapter is determined by usual S3 polymorphism. On the other
#'		hand, if x is a call for a modeling function, inheritance of the class
#'		is determined by function name in the function call. In such case, this
#'		function call model.adapter.CLASS_NAME() to initialize object.
#'		
#'	@section Adding support for new function
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
model.adapter <- function(x) {
	if (is.object(x)) {
		original.call <- substitute(x)
	} else {
		original.call <- x
	}
	if (is.call(original.call)) {
		fun.name <- get.function(original.call, "character")
		code <- sprintf(
			"model.adapter.%s(original.call)", get.class.name(fun.name)
		)
		object <- eval(parse(text = code))
		return(object)
	}
	UseMethod("model.adapter")
}


#-------------------------------------------------------------------------------
#'	Abstraction layer for model functions/objects.
#'
#'	This class encapsulates differences in specifications of statistical/machine
#'	learning functions and model objects to provide standard way to access 
#'	data and methods of models. To add support for a new modeling function,
#'	new generator object of a reference class inheriting this class must be 
#'	implimented and methods that cannot work well with the model should
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
#'	@field family
#'		a read-only character of family name. If a model which does not use
#'		family, this field is character(0).
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
#			family:
#				モデルのファミリーを表す文字列。
#				familyがないモデルの場合はcharacter(0)。
#		Methods:
#			以下を参照。
#-------------------------------------------------------------------------------
model.adapter.default <- setRefClass(
	"model.adapter",
	fields = list(
		src = "list",
		family = "character"
	)
)


#-------------------------------------------------------------------------------
#	コンストラクタ
#	Args:
#		x: モデルオブジェクトもしくはモデルの呼び出しを表すcall。
#-------------------------------------------------------------------------------
model.adapter.default$methods(
	initialize = function(x) {
		"
		Initialize class
		@param x model object or function call 
		"
		# Initialize src field. / srcフィールドの初期化。
		if (is.object(x)) {
			original.call <- substitute(x)
		} else {
			original.call <- x
		}
		if (is.call(original.call)) {
			src$call <<- original.call
		} else {
			src$object <<- x
		}
		# Initialize family field. / familyフィールドの初期化。
		family.name <- .self$get.family(x)
		if (!is.null(family.name)) {
		   	family <<- format.family(family.name, "character")
		}
	}
)


#-------------------------------------------------------------------------------
#	モデルオブジェクトを返す。
#-------------------------------------------------------------------------------
model.adapter.default$methods(
	object = function() {
		"Return model object."
		return(src$object)
	}
)


#-------------------------------------------------------------------------------
#	関数呼び出しを返す。
#-------------------------------------------------------------------------------
model.adapter.default$methods(
	call = function() {
		"Return function call."
		return(src$call)
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
		@param x call or model object.
		@return family of the model. If family was not specified, return NULL.
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
		Get call from model object.
		@param x a model object.
		@return call by which the object is made.
		"
		if (isS4(x)) {
		   	result <- x@call
		} else {
		   	result <- x$call
		}
		return(result)
	}
)


		if (!is.null(src$call)) {
			return(src$call$family)
		} else {
			if (isS4(src$object)) {
				if ("family" %in% slotNames(src$object)) {
					return(src$object@family)
				} else {
					return(NULL)
				}
			} else {
				return(src$object$family)
			}
		}
	}
)

model.adapter.default$lock("src")
model.adapter.default$lock("family")

