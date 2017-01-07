#------------------------------------------------------------------------------
#	gamm関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
#'	model.interface class for gamm
#'
#'	This reference class contains methods for \code{\link[mgcv]{gamm}} in
#'	\emph{mgcv} package.
#'	Note that because an object of gamm does not keep original call,
#'	get.call() function always returns NULL. Also, when an instance of this
#'	class is made from model object, 'call' field is always call("<undef>").
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.gamm
#'	@exportClass model.interface.gamm
#------------------------------------------------------------------------------
model.interface.gamm <- setRefClass(
	"model.interface.gamm", contains = "model.interface"
)


#------------------------------------------------------------------------------
#	モデルオブジェクトからcallを取得する。
#------------------------------------------------------------------------------
model.interface.gamm$methods(
	get.call = function(x) {
		return(NULL)
	}
)


#------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#------------------------------------------------------------------------------
model.interface.gamm$methods(
	get.family = function(x, type = c("character", "family")) {
		type <- match.arg(type)
		if (is.call(x)) {
			family <- x$family
		} else {
			family <- x$gam$family
		}
		return(format.family(family, type))
	}
)


#------------------------------------------------------------------------------
#	モデルのformulaを取得する。
#------------------------------------------------------------------------------
model.interface.gamm$methods(
	get.formula = function(x, envir = parent.frame()) {
		f <- callSuper(x, envir)
		if (is.null(f)) {
			f <- x$gam$formula
		}
		return(f)
	}
)


#------------------------------------------------------------------------------
#	モデルのdataを取得する。
#------------------------------------------------------------------------------
model.interface.gamm$methods(
	get.data = function(x, envir = parent.frame()) {
		if (is.call(x)){
			return(callSuper(x, envir))
		} else {
			d <- x$gam$model
			var.names <- names(attr(x$gam$terms, "dataClasses"))
			d <- d[var.names]
			return(d)
		}
	}
)


#------------------------------------------------------------------------------
#	formulaの.を展開する。
#------------------------------------------------------------------------------
model.interface.gamm$methods(
	expand.formula = function(f, d, specials = c("s", "te", "ti", "t2")) {
		callSuper(f, d, specials)
	}
)


#------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#------------------------------------------------------------------------------
model.interface.gamm$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)


