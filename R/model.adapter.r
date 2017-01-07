#-------------------------------------------------------------------------------
#	モデリング関数の違いを吸収するReference Class、model.adapterクラスの
#	ジェネレーターオブジェクト。
#	内部にmodel.interfaceクラスのオブジェクトをもち、各モデルへの対応は
#	model.interfaceに任せる。
#
#		Fields:
#			src: 初期化に使った情報を格納。
#				src$call: 初期化に使ったcall。
#				src$object: 初期化に使ったオブジェクト。
#				callで初期化された場合、src$objectはNULL。
#				objectで初期化された場合、src$callはNULL。
#			call:
#				モデルの呼び出し式。
#			object:
#				評価されたモデルオブジェクト。
#			env:
#				callを評価する環境。
#			family:
#				モデルのファミリーを表す文字列。
#				familyがないモデルの場合はcharacter(0)。
#			link:
#				リンク関数。
#			linkinv:
#				リンク関数の逆関数。
#			formula:
#				モデル式。
#			data:
#				モデル作成に使われたデータ。
#				モデルオブジェクトはデータを保持していないことがあるので、
#				元のデータフレームが取得できないことがある。
#			interface:
#				model.interfaceクラスのオブジェクト。
#				様々なモデルへの対応はこのオブジェクトに頼る。
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
#'	@field object
#'		a read-only object of a model. If model.adapter is initialized by a
#'		call for a model function, this field is initialized by NULL.
#'		Because some methods like get.intercept(), predict(), etc. require
#'		model object, such methods evaluate 'call' field and store the
#'		resultant object into this field.
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
#'	@field link
#'		a function object of link function of the model. If the model does
#'		not have link function, this field is \code{\link[base]{identity}}
#'		function.
#'
#'	@field linkinv
#'		a function object of inverse link function of the model. If the model
#'		does not have link function, this field is
#'		\code{\link[base]{identity}} function.
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
#'
#'	@export model.adapter
#'	@include model.interface.default.r
#-------------------------------------------------------------------------------
model.adapter <- setRefClass(
	"model.adapter",
	fields = list(
		src = "list",
		call = "call",
		object = "ANY",
		env = "environment",
		family = "character",
		link = "function",
		linkinv = "function",
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
		.self$init.interface(seed)
		.self$init.src(seed)
		.self$init.call(seed)
		.self$init.object(seed)
		env <<- envir
		.self$init.family(seed)
		.self$init.link(seed)
		.self$init.data(seed, data)
		.self$init.formula(seed)
	}
)


#-------------------------------------------------------------------------------
#	interfaceフィールドを初期化する。
#-------------------------------------------------------------------------------
model.adapter$methods(
	init.interface = function(seed) {
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
			interface <<- model.interface(seed)
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
#	callフィールドを初期化する。
#-------------------------------------------------------------------------------
model.adapter$methods(
	init.call = function(seed) {
		"
		Initialize call field.
		"
		if (is.call(seed)) {
			call <<- match.generic.call(seed)
		} else {
			if (!is.null(interface$get.call(seed))) {
				call <<- match.generic.call(interface$get.call(seed))
			}
		}
	}
)


#-------------------------------------------------------------------------------
#	objectフィールドを初期化する。
#-------------------------------------------------------------------------------
model.adapter$methods(
	init.object = function(seed) {
		"
		Initialize object field.
		"
		if (is.call(seed)) {
			object <<- NULL
		} else {
			object <<- seed
		}
	}
)


#-------------------------------------------------------------------------------
#	familyフィールドを初期化する。
#-------------------------------------------------------------------------------
model.adapter$methods(
	init.family = function(seed) {
		"
		Initialize family field.
		"
		family.name <- interface$get.family(seed, "character")
		if (!is.null(family.name)) {
			family <<- family.name
		}
	}
)


#-------------------------------------------------------------------------------
#	dataフィールドを初期化する。
#-------------------------------------------------------------------------------
model.adapter$methods(
	init.data = function(seed, data) {
		"
		Initialize data field.
		"
		if (is.null(data)) {
			d <- interface$get.data(seed, envir = .self$env)
			if (!is.null(d)) {
				data <<- interface$get.data(seed)
			}
		} else {
			data <<- data
		}
		if (!.self$has.data()) {
			warning(
				"Couldn't retrieve data from the call/object. This may cause errors.
				 To use full functionality of the class, please specify 'data' argument to supply data."
			)
		}
	}
)


#-------------------------------------------------------------------------------
#	formulaフィールドを初期化する。
#-------------------------------------------------------------------------------
model.adapter$methods(
	init.formula = function(seed) {
		"
		Initialize formula field.
		"
		if (.self$has.call()) {
			formula <<- interface$get.formula(call, .self$env)
		} else {
			formula <<- interface$get.formula(src$object, .self$env)
		}
		formula <<- interface$expand.formula(formula, .self$data)
	}
)


#-------------------------------------------------------------------------------
#	linkフィールドとlinkinvを初期化する。
#-------------------------------------------------------------------------------
model.adapter$methods(
	init.link = function(seed) {
		"
		Initialize link and linkinv fields.
		"
		link <<- interface$get.link(seed)
		linkinv <<- interface$get.linkinv(seed)
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


#-------------------------------------------------------------------------------
#	予測値を計算してma.prediction型のオブジェクトを返す。
#-------------------------------------------------------------------------------
model.adapter$methods(
	predict.types = function() {
		"
		Return a character vector representing conversion table of 'type'
		argument of predict() method.
		"
		return(.self$interface$predict.types())
	}
)


model.adapter$methods(
	predict = function(newdata = NULL, ...) {
		"
		Calculate prediction and return a
		\\code{\\link{ma.prediction}} object.
		"
		# If object field is NULL, make it from call.
		# objectフィールドがNULLだったらcallを評価して作成する。
		if (is.null(.self$object)) {
		   	.self$object <- eval(.self$src$call, .self$src$envir)
		}
		pred <- .self$interface$predict(.self$object, newdata = newdata, ...)
		pred <- ma.prediction(
			pred, fixed = newdata[.self$x.names(type = "base")]
		)
		return(pred)
	}
)


#-------------------------------------------------------------------------------
#	モデルの切片を返す。切片がないモデルはNULLを返す。
#-------------------------------------------------------------------------------
model.adapter$methods(
	intercept = function() {
		"
		Returns estimated intercept of the model.
		If the model does not have intercept, this function returns NULL.
		"
		if (is.null(.self$object)) {
			.self$object <- eval(.self$src$call, .self$src$envir)
		}
		return(.self$interface$get.intercept(.self$object))
	}
)


#-------------------------------------------------------------------------------
#	Lock all fields other than "object" field.
#	objectフィールド以外のフィールドをロックする。
#-------------------------------------------------------------------------------
for (field.name in names(model.adapter$fields)) {
	if (field.name != "object") {
	   	model.adapter$lock(n)
	}
}
rm(field.name)


