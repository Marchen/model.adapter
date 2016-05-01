library(testthat)

#-------------------------------------------------------------------------------
#' Test initialization of model.adapter
#'
#' @param call call for a function for testing.
#' @param function.name character literal of function name.
#' @param env environment in which call is evaluated.
#'
#-------------------------------------------------------------------------------
test__initialize <- function(call, function.name, env = parent.frame()) {
	class.name <- get.class.name(function.name)
	test_that(
		sprintf("Initialization of model.adapter.%s by call", class.name),
		{
			adapter <- model.adapter(call)
			expect_is(adapter, sprintf("model.adapter.%s", class.name))
		}
	)
	test_that(
		sprintf("Initialization of model.adapter.%s by object", class.name),
		{
			object <- eval(call, envir = env)
			adapter <- model.adapter(object)
			expect_is(adapter, sprintf("model.adapter.%s", class.name))
		}
	)
}


#-------------------------------------------------------------------------------
#' Run all available test of model.adapter
#'
#' @param call call for a function to test.
#' @param function.name character literal of function name to test.
#'
#' @export
#'
#' @examples
#'	test__all(substitute(lm(Sepal.Length ~ ., data = iris)), "lm")
#-------------------------------------------------------------------------------
test__all <- function(
	call, function.name, package.name = find.package(function.name)
) {
	# Load package
	library(package.name, character.only = TRUE)
	# Run tests
	test__initialize(call, function.name, parent.frame())
	# Unload package
	if (package.name != "stats") {
		unloadNamespace(package.name)
	}
}




