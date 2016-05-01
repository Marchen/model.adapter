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
	if (!is.call(x)) {
	   	original.call <- substitute(x)
	} else {
	   	original.call <- x
	}
	if (is.call(original.call)) {
	   	fun.name <- as.character(original.call[1])
	   	code <- sprintf("model.adapter.%s(x)", fun.name)
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
#'	@field call
#'		a call for modeling function used for initialization of the class.
#'	@field object.data
#'		an object of a model used for initialization of the class.
#'	@field class.name
#'		class name of the original model object.
#'	@export
#-------------------------------------------------------------------------------
#	モデリング関数の違いを吸収するReference Class、model.adapterクラスの
#	ジェネレーターオブジェクト。
#	この基底クラスを継承して、様々なモデルに対応させる。
#
#		Fields:
#			call: 初期化に使ったcall。
#			object.data: 初期化に使ったモデルの結果オブジェクト。
#			class.name: モデルオブジェクトのクラス名。
#		Methods:
#			以下を参照。
#-------------------------------------------------------------------------------
model.adapter.default <- setRefClass(
	"model.adapter",
	fields = list(
		call = "call",
		object.data = "list",
		class.name = "character"
	)
)


#-------------------------------------------------------------------------------
#	コンストラクタ
#	Args:
#		x: モデルオブジェクトもしくはモデルの呼び出しを表すcall。
#-------------------------------------------------------------------------------
model.adapter.default$methods(
	initialize = function(x) {
		"Initialize class
		@param x model object or function call"
		if (!is.call(x)) {
		   	original.call <- substitute(x)
		} else {
		   	original.call <- x
		}
		if (is.call(original.call)) {
		   	call <<- original.call
		} else {
			object.data <<- unclass(x)
			class.name <<- class(x)
		}
	}
)


#	モデルのfamilyを取得する。
#-------------------------------------------------------------------------------
model.adapter.default$methods(
	family = function() {
		"Get family. If family was not specified, return NULL."
		if (!is.null(call)) {
		   	return(call$family)
		} else {
		   	return(object$family)
		}
	}
)


