#------------------------------------------------------------------------------
#'	Get package name from function name or object.
#'
#'	@param x function name, call or model object.
#'	@param envir an environment where \code{x} is evaluated.
#'
#'	@return a character representing package name.
#'
#'	@examples
#'	# Get package name from function name
#'	package.name("cforest")
#'	# Get package name from function call.
#'	package.name(substitute(ctree(Petal.Length~., data=iris)))
#'	# Get package name from object.
#'	data(iris)
#'	object <- lm(Petal.Length ~ ., data = iris)
#'	package.name(object)
#------------------------------------------------------------------------------
#	関数名やクラスのオブジェクト名からその関数/そのオブジェクトを生成した関数が
#	含まれるパッケージ名を取得する関数。対応してない関数は関数名やクラス名の
#	１つめがパッケージ名だと仮定してパッケージ名を返す。
#
#	Args:
#		x: 関数名、関数の呼び出し、もしくはモデルオブジェクト。
#		envir: 関数呼び出しを評価する環境。
#
#	Value:
#		その関数が含まれる/そのオブジェクトを作成した関数が含まれるパッケージ名
#		を表す文字列。
#------------------------------------------------------------------------------
package.name <- function(x, envir){
	UseMethod("package.name", x)
}

#-------------------------------------------------------------------------------
#'	@describeIn package.name Default S3 method.
#'	@method package.name default
#-------------------------------------------------------------------------------
package.name.default <- function(x, envir){
	# gamはglmやlmを引き継いでるので、先に評価
	if (is(x, "gam")){
		return(ifelse(is.null(x$mgcv.conv), "gam", "mgcv"))
	}
	if (is(x, "lm")) return("stats")
	if (is(x, "lme")) return("nlme")
	if (is(x, "lmerMod")) return("lme4")
	if (is(x, "glmerMod")) return("lme4")
	if (is(x, "glmmadmb")) return("glmmADMB")
	if (is(x, "BinaryTree")) return("party")
	if (is(x, "RandomForest")) return("party")
	if (is(x, "randomForest")) return("randomForest")
	if (is(x, "gbm")) return("gbm")
	if (is(x, "svm")) return("e1071")
	if (is(x, "tree")) return("tree")
	if (is(x, "rpart")) return("rpart")
	if (is(x, "gamm")) return("mgcv")
	return(class(x)[1])
}

#-------------------------------------------------------------------------------
#'	@describeIn package.name Method for character.
#'	@method package.name character
#-------------------------------------------------------------------------------
package.name.character <- function(x, envir) {
	fun <- try(get(x, envir = envir), silent = TRUE)
	if (class(fun) != "try-error") {
		package <- environmentName(environment(fun))
	} else {
		package <- switch(
			x,
			cforest = "party",
			ctree = "party",
			lm = "stats",
			glm = "stats",
			glmmadmb = "glmmADMB",
			lme = "nlme",
			lmer = "lme4",
			glmer = "lme4",
			svm = "e1071",
			gam = "mgcv",
			gamm = "mgcv",
			x
		)
	}
	return(package)
}

#-------------------------------------------------------------------------------
#'	@describeIn package.name Method for call.
#'	@method package.name call
#-------------------------------------------------------------------------------
package.name.call <- function(x, envir) {
	fun <- get.function(x, "function", envir)
	package <- environmentName(environment(fun))
	return(package)
}
