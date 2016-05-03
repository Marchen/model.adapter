library(testthat)

#-------------------------------------------------------------------------------
#'	Test for initialization of model.adapter
#'
#'	@param env environment in which call is evaluated.
#'	@inheritParams test__all
#'
#'	@example
#'	test__initialize(lm(Sepal.Length ~ ., data = iris), "lm")
#'	object <- lm(Sepal.Length ~ ., data = iris)
#'	test__initialize(object, "lm")
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
#'	Test for family() function.
#'
#'	@export
#'	@inheritParams test__all
#'	@inheritParams test__initialize
#'
#'	@examples
#'	test__family(
#'		glm(Sepal.Length ~ ., data = iris, family = gaussian),
#'		"lm", family = "gaussian"
#'	)
#-------------------------------------------------------------------------------
test__family <- function(
	call, function.name, family = NULL, env = parent.frame()
) {
	class.name <- get.class.name(function.name)
	test_that(
		sprintf("Get family of model.adapter.%s by call", class.name),
		{
			adapter <- model.adapter(call)
			f <- adapter$family
			if (!is.null(family)) {
				f <- format.family(f, "character")
				expect_equal(f, family)
			} else {
				expect_is(f, "character")
				expect_length(f, 0)
			}
		}
	)
	test_that(
		sprintf("Get family of model.adapter.%s by object", class.name),
		{
			object <- eval(call, envir = env)
			adapter <- model.adapter(object)
			f <- adapter$family
			if (!is.null(family)) {
			   	f <- format.family(f, "character")
			   	expect_equal(f, family)
			} else {
				expect_is(f, "character")
				expect_length(f, 0)
			}
		}
	)
}



#-------------------------------------------------------------------------------
#'	Run all available test of model.adapter
#'
#'	@param call call for a function to test.
#'	@param function.name character literal of function name to test.
#'	@param family a character literal of family name.
#'
#'	@export
#'
#'	@examples
#'	test__all(
#'		substitute(glm(Sepal.Length ~ ., data = iris, family = gaussian)),
#'		function.name = "lm",
#'		family = "gaussian"
#'	)
#-------------------------------------------------------------------------------
test__all <- function(
	call, function.name, package.name = find.package(function.name),
	family = NULL
) {
	# Load package
	library(package.name, character.only = TRUE)
	# Run tests
	test__initialize(call, function.name, parent.frame())
	test__family(call, function.name, family, parent.frame())
	# Unload package
	if (package.name != "stats") {
		unloadNamespace(package.name)
	}
}




