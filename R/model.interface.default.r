#------------------------------------------------------------------------------
#	model.interfaceクラスを初期化する。
#
#	Args:
#		x:
#			サポートされているモデルのオブジェクト。
#			S3メソッドの決定にしか使われない。
#------------------------------------------------------------------------------
#'	Initialize model.interface class.
#'
#'	This function makes an object of a derived class of \emph{model.interface}
#'	class that abstracts differences in specifications of supported modeling
#'	functions.
#'
#'	@param x = NULL
#'		an object of supported models.
#'		This is only used for dispatching S3 methods.
#'
#'	@return
#'		an object of derived class of \code{\link{model.interface-class}}.
#'
#'	@section Adding support for new function:
#'		To be continued...
#'
#'	@family model.interface
#'	@export
#------------------------------------------------------------------------------
model.interface <- function(x = NULL) {
	UseMethod("model.interface")
}


#------------------------------------------------------------------------------
#	モデル抽象化レイヤー
#------------------------------------------------------------------------------
#'	An interface for model.adapter and statistical/machine learning models.
#'
#'	This class provides unified interface for calls/objects of
#'	statistical/machine learning models.
#'	\code{\link{model.adapter-class}{model.adapter}} internally using object
#'	of this class to get information from model calls/objects.
#'
#'	@export
#------------------------------------------------------------------------------
model.interface.default <- setRefClass(
	"model.interface",
	methods = list(initialize = function(x) { })
)


#------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#	Args:
#		x: 関数呼び出しのcall、もしくはモデルオブジェクト。
#------------------------------------------------------------------------------
model.interface.default$methods(
	get.family = function(x, type = c("character", "family")) {
		"
		Get family from call or model object.
		If family was not specified, return NULL.
		\\describe{
			\\item{\\code{x}}{call or model object.}
			\\item{\\code{type = c(\"character\", \"family\")}}{
				a character literal specifying the type of data returned.
				If \"character\", this returns character vector of family name.
				If \"family\", this returns \\code{\\link{family}} object.
				For some family specific for certain model (e.g., categorical
				family of MCMCglmm), their family object is not implimented.
				For such family, this method raise stop error.
			}
		}
		"
		type <- match.arg(type,)
		if (is.call(x)) {
			family <- x$family
		} else {
			if (isS4(x)) {
				if ("family" %in% slotNames(x)) {
					family <- src$object@family
				} else {
					return(NULL)
				}
			} else {
				family<- x$family
			}
		}
		if (!is.null(family)) {
			return(format.family(family, type))
		} else {
			return(NULL)
		}
	}
)


#------------------------------------------------------------------------------
#	モデルのcallを取得する。
#	Args:
#		x: はモデルオブジェクト。
#	Value:
#		オブジェクトを作ったcall。
#------------------------------------------------------------------------------
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



#------------------------------------------------------------------------------
#	モデルのdataを取得する。
#	Args:
#		x: 関数呼び出しのcall、もしくはモデルオブジェクト。
#		envir: xに入ったcallを評価する環境。
#------------------------------------------------------------------------------
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


#------------------------------------------------------------------------------
#	モデルのformulaを取得する。
#	Args:
#		x: 関数呼び出しのcall、もしくはモデルオブジェクト。
#		envir: xに入ったcallを評価する環境。
#------------------------------------------------------------------------------
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
				f <- eval(x@call$formula, envir)
			} else {
				f <- eval(x$call$formula, envir)
			}
		} else {
			if (!is.null(x$formula)) {
				f <- eval(x$formula, envir)
			} else {
				args <- lapply(as.list(x), eval, envir = envir)
				f <- args[sapply(args, is.formula)][[1]]
			}
		}
		# Because MASS::stepAIC converts formula field of lm, glm, lme
		# object to terms object, use formula() to convert terms to
		# formula.
		if (!is.null(f)) {
			f <- formula(f)
		}
		return(f)
	}
)


#------------------------------------------------------------------------------
#	formulaの.を展開する。
#------------------------------------------------------------------------------
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


#------------------------------------------------------------------------------
#	予測値を計算して、ma.prediction型オブジェクトを返す。
#------------------------------------------------------------------------------
model.interface.default$methods(
	predict = function(object, newdata = NULL, ...) {
		"
		Calculate predictions and returns \\code{\\link{ma.prediction}} object.
		\\describe{
			\\item{\\code{object}}{a model object used for prediction.}
			\\item{\\code{newdata = NULL}}{
				a data.frame containing data used for prediction.
			}
			\\item{\\code{...}}{other variables passed to predict methods.}
		}
		"
		pred <- stats::predict(object, newdata = newdata, ...)
		# Make ma.prediction object.
		# ma.predicitonオブジェクトの作成。
		args <- as.list(match.call())[-1]
		result <- ma.prediction(
			pred, args$type, NULL, args$interval, args$level
		)
		return(result)
	}
)


#------------------------------------------------------------------------------
#	モデルオブジェクトから切片の推定値を取得する。
#------------------------------------------------------------------------------
model.interface.default$methods(
	get.intercept = function(object) {
		"
		Get intercept from model object.
		If intercept is not available for the model, this should return NULL.
		\\describe{
			\\item{\\code{object}}{
				a model object from which estimated value of intercept is
				extracted.
			}
		}
		"
		return(object$coefficients["(Intercept)"])
	}
)


#------------------------------------------------------------------------------
#	リンク関数を返す。
#------------------------------------------------------------------------------
model.interface.default$methods(
	get.link = function(x) {
		"
		Get link function. If the model does not have link function, this
		function returns \\code{\\link[base]{identity}} function.
		\\describe{
			\\item{\\code{x}}{
				an object of statistical model or a call of model function.
			}
		}
		"
		# because gaussian()$linkfun returns function(mu) mu and it's not
		# identical to identity() function(x) x
		# use gaussian()$linkfun instead of identity
		return(gaussian()$linkfun)
	}
)


#------------------------------------------------------------------------------
#	リンク関数の逆関数を返す。
#------------------------------------------------------------------------------
model.interface.default$methods(
	get.linkinv = function(x) {
		"
		Get inverse function of link function. If the model does not have
		link function, this function returns \\code{\\link[base]{identity}}
		function.
		\\describe{
			\\item{\\code{x}}{
				an object of statistical model or a call of model function.
			}
		}
		"
		return(gaussian()$linkinv)
	}
)


#------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#------------------------------------------------------------------------------
model.interface.default$methods(
	predict.types = function() {
		"
		Return a character vector representing conversion table of 'type'
		argument of predict() method.
		"
		return(make.predict.types())
	}
)


