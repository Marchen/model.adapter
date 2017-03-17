#------------------------------------------------------------------------------
#	MCMCglmm関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
#'	model.interface class for MCMCglmm
#'
#'	This reference class contains methods for \code{\link[MCMCglmm]{MCMCglmm}}
#'	in \emph{MCMCglmm} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.MCMCglmm
#'	@exportClass model.interface.MCMCglmm
#------------------------------------------------------------------------------
model.interface.MCMCglmm <- setRefClass(
	"model.interface.MCMCglmm", contains = "model.interface"
)


#------------------------------------------------------------------------------
#	モデルのdataを取得する。
#------------------------------------------------------------------------------
model.interface.MCMCglmm$methods(
	get.data = function(x, envir = parent.frame(), package = "", ...) {
		if (is.call(x)) {
			callSuper(x, envir, package, ...)
		} else {
			return(data.frame())
		}
	}
)


#------------------------------------------------------------------------------
#	formulaを取り出し。
#------------------------------------------------------------------------------
model.interface.MCMCglmm$methods(
	get.formula = function(x, envir = parent.frame()) {
		if (is.call(x)) {
			return(callSuper(x, envir))
		} else {
			return(x$Fixed$formula)
		}
	}
)


#------------------------------------------------------------------------------
#	モデルのfamilyを取得する。
#------------------------------------------------------------------------------
model.interface.MCMCglmm$methods(
	get.family = function(x, type = c("character", "family")) {
		# Get family
		if (is.call(x)) {
			family <- family.from.call(x)
		} else {
			family <- x$family[[1]]
		}
		family <- gsub("multinomial.*", "multinomial", family)
		# Convert family object to character.
		if (type == "character") {
			if (class(family) != "character") {
				family <- format.family(family, type)
			}
			return(family)
		}
		if (type == "family") {
			result <- try(format.family(family, type))
			if (class(result) == "try-error") {
				msg <- sprintf(
					"'%s' family object is not supported by MCMCglmm.", family
				)
				stop(msg)
			} else {
				family <- result
			}
		}
		return(family)
	}
)


#------------------------------------------------------------------------------
#	モデルオブジェクトからcallを取得する。
#------------------------------------------------------------------------------
model.interface.MCMCglmm$methods(
	get.call = function(x) {
		return(NULL)
	}
)


#------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#------------------------------------------------------------------------------
model.interface.MCMCglmm$methods(
	predict.types = function() {
		type <- make.predict.types(
			link = "terms", prob = "response", class = "response"
		)
		return(type)
	}
)


#------------------------------------------------------------------------------
#	リンク関数を返す。
#------------------------------------------------------------------------------
model.interface.MCMCglmm$methods(
	get.link = function(x) {
		f <- .self$get.family(x, "character")
		check.supported.family(f)
		link <- switch(
			f,
			gaussian = identity,
			poisson = log,
			categorical = binomial()$linkfun,
			multinomial = binomial()$linkfun,
			geometric = binomial()$linkfun,
			exponential = function(x) -log(x)
		)
		return(link)
	}
)


#------------------------------------------------------------------------------
#	リンク関数の逆関数を返す。
#------------------------------------------------------------------------------
model.interface.MCMCglmm$methods(
	get.linkinv = function(x) {
		f <- .self$get.family(x, "character")
		check.supported.family(f)
		link <- switch(
			f,
			gaussian = identity,
			poisson = exp,
			categorical = binomial()$linkinv,
			multinomial = binomial()$linkinv,
			geometric = binomial()$linkinv,
			exponential = function(x) exp(-x)
		)
		return(link)
	}
)


#------------------------------------------------------------------------------
#	familyがサポートされてるかをチェックする。
#------------------------------------------------------------------------------
#' (Internal) Check if the specified family of MCMCglmm is supported.
#'
#'	@param a character vector of length one specifing
#------------------------------------------------------------------------------
check.supported.family <- function(family) {
	supported <- c(
		"gaussian", "poisson", "categorical", "multinomial", "geometric",
		"exponential"
	)
	if (any(!family %in% supported)) {
		msg <- paste(supported[1:(length(supported) - 1)], collapse = ", ")
		msg <- paste(msg, "and", supported[length(supported)])
		msg <- sprintf(
			"Currently, only MCMCglmm with %s families are supported.
			\nIf you have any information about link function of other families,
			 please teach me.",
			msg
		)
		stop(msg)
	}
}




