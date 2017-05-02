#------------------------------------------------------------------------------
#	glmmML関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
#'	model.interface class for glmmML
#'
#'	This reference class contains methods for \code{\link[glmmML]{glmmML}} in
#'	\emph{glmmML} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.glmmML
#'	@exportClass model.interface.glmmML
#------------------------------------------------------------------------------
model.interface.glmmadmb <- setRefClass(
	"model.interface.glmmadmb", contains = "model.interface"
)


#------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#	familyオブジェクトがサポートされていない場合、エラーを投げる。
#------------------------------------------------------------------------------
model.interface.glmmadmb$methods(
	get.family = function(x, type = c("character", "family")) {
		# Get family character.
		if (is.call(x)) {
			family <- family.from.call(x)
		} else {
			family <- x$family
		}
		family <- gsub("^binom$", "binomial", family)
		family <- gsub("^gamma$", "Gamma", family)
		if (type == "character") {
			return(family)
		}
		# Convert family character to family object.
		result <- try(format.family(family, type))
		if (class(result) == "try-error") {
			msg <- sprintf(
				"'%s' family object is not supported by glmmadmb.", family
			)
			stop(msg)
		}
		return(result)
	}
)


#------------------------------------------------------------------------------
#	モデル作成に使われたデータを返す。
#------------------------------------------------------------------------------
model.interface.glmmadmb$methods(
	get.data = function(x, envir = parent.frame(), package = "", ...) {
		if (is.call(x)) {
			return(callSuper(x, envir, package, ...))
		} else {
			d <- x$frame
			attr(d, "terms") <- NULL
			return(d)
		}
	}
)


#------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#------------------------------------------------------------------------------
model.interface.glmmadmb$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)


#------------------------------------------------------------------------------
#	予測値を計算する。
#------------------------------------------------------------------------------
model.interface.glmmadmb$methods(
	predict = function(object, newdata, type, ...) {
		# Change 'fixed' field of object to handle '.' in the formula.
		object$fixed <- .self$expand.formula(object$formula, object$frame)
		callSuper(object, newdata, type, ...)
	}
)

