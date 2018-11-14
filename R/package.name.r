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
package.name <- function(x, envir){
	UseMethod("package.name", x)
}

#-------------------------------------------------------------------------------
#'	@describeIn package.name Default S3 method.
#'	@method package.name default
#-------------------------------------------------------------------------------
package.name.default <- function(x, envir){
	# Because Gam class inherts glm and lm, evaluate Gam first.
	if (is(x, "Gam")) return("gam")
	if (is(x, "gam")) return("mgdv")
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
