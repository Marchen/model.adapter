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
#'	@field interface
#'		an object of model.interface class or subclass of model.interface.
#'
#'	@section For developpers:
#'		If x is an object containing result of a statistical models, 
#'		inheritance of the class is determined by usual S3 polymorphism. 
#'		On the other hand, if x is a call for a modeling function, inheritance 
#'		of the class is determined by function name in the function call. 
#'		In such case, this function call model.adapter.CLASS_NAME() to 
#'		initialize object.
#'	@export model.adapter
#'	@include model.interface.default.r
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
			\\item{\\code{x}}{
				an object of supported models or a call for a model function.
			}
			\\item{\\code{envir = parent.frame(4L)}}{
				envir an environment in which call in x is evaluated.
			}
			\\item{\\code{data}}{data.frame used for further manipulations.}
			\\item{\\code{...}}{arguments to be passed to methods.}
		}
		"
		seed <- make.call.or.object(substitute(x), envir)
		# Initialize interface field. / interfaceフィールドの初期化
		.self$init.interface(seed, x)
		# Initialize src field. / srcフィールドの初期化。
		.self$init.src(seed)
		# Initialize call field. / callフィールドの初期化。
		if (is.call(seed)) {
			call <<- match.generic.call(seed)
		} else {
			if (!is.null(interface$get.call(seed))) {
				call <<- match.generic.call(interface$get.call(x))
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


#-------------------------------------------------------------------------------
#	interfaceフィールドを初期化する。
#-------------------------------------------------------------------------------
model.adapter$methods(
	init.interface = function(seed, x) {
		"
		Initialize interface field.
		"
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
	}
)


#-------------------------------------------------------------------------------
#	srcフィールドを初期化する。
#-------------------------------------------------------------------------------
model.adapter$methods(
	init.src = function(seed) {
		"
		Initialize src field.
		"
		if (is.call(seed)) {
			src$call <<- seed
		} else {
			src$object <<- seed
		}
	}
)


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


#-------------------------------------------------------------------------------
#	説明変数の一覧を取得する。
#	Args:
#		specials: termsに渡される引数。
#		type:
#			もし"all"だったら高次の項なども返す。
#			もし"base"だったら一次の項だけを返す。
#-------------------------------------------------------------------------------
model.adapter$methods(
	x.names = function(specials = NULL, type = c("all", "base")) {
		"
		Get name of explanatory variables.
		For details, see \\code{\\link[model.adapter]{x.names}}
		\\describe{
			\\item{\\code{specials = NULL}}{
				special characters to be passed to \\link{terms}.
			}
			\\item{\\code{type = c(\"all\", \"base\")}}{
				if \"all\", this function returns all explanatory variables
				including interactions, higher order terms, splines, etc.
				If \"base\" only basic form of the variables are returned.
			}
		}
		"
		type <- match.arg(type)
		model.adapter::x.names(formula, data, specials, type)
	}
)


#-------------------------------------------------------------------------------
#	応答変数の名前を取得する。
#-------------------------------------------------------------------------------
model.adapter$methods(
	y.names = function() {
		"
		Get name of response variables.
		"
		as.character(formula[2])
	}
)


#-------------------------------------------------------------------------------
#	説明変数のデータを取得する。
#-------------------------------------------------------------------------------
model.adapter$methods(
	x.vars = function() {
		"
		Get data of explanatory variables.
		"
		return(data[x.names(type = "base")])
	}
)


#-------------------------------------------------------------------------------
#	応答変数のデータを取得する。
#-------------------------------------------------------------------------------
model.adapter$methods(
	y.vars = function() {
		"
		Get data of response variable.
		"
		return(data[y.names()])
	}
)



for (field.name in names(model.adapter$fields)) {
	model.adapter$lock(n)
}
rm(field.name)


