#------------------------------------------------------------------------------
#'	(Internal) Find class name of a model object from function name
#'
#'	If specified \code{function.name} is not supported, this function assumes
#'	that class name is same as the \code{function.name}.
#'
#'	@param function.name a character literal of function name.
#'	@param package.name a character literal of package name.
#'
#'	@return a character literal of class name
#'
#'	@examples
#'	get.class.name("cforest")
#------------------------------------------------------------------------------
get.class.name <- function(function.name, package.name){
	class.name <- switch(
		function.name,
		cforest	= "RandomForest",
		ctree	= "BinaryTree",
		lmer	= "lmerMod",
		glmer	= "glmerMod",
		gam		= ifelse(package.name == "gam", "Gam", "gam")
	)
	if (is.null(class.name)){
		class.name <- function.name
	}
	return(class.name)
}

