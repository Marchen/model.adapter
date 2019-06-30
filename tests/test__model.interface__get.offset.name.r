#==============================================================================
#	Test for has.offset method in model.interface.
#==============================================================================
library(testthat)
library(model.adapter)


#------------------------------------------------------------------------------
#	Create test data.
#------------------------------------------------------------------------------
test.data <- list(
	# Test formula with no offset.
	call.no.offset = list(
		x = substitute(glm(Petal.Length ~ ., data = iris)),
		expected = character()
	),
	# Test formula with two offset without and with log.
	call.formula.1 = list(
		x = substitute(
			glm(
				Petal.Length ~ . + offset(iris$Sepal.Width)
				+ offset(log(iris$Petal.Width)),
				data = iris
			)
		),
		expected = c("iris$Sepal.Width", "iris$Petal.Width")
	),
	# Test formula without '$'.
	call.formula.2 = list(
		x = substitute(
			glm(Petal.Length ~ . + offset(Sepal.Width), data = iris)
		),
		expected = c("Sepal.Width")
	),
	# Test argument with '$'
	call.argument.1 = list(
		x = substitute(
			glm(Petal.Length ~ ., offset = iris$Sepal.Width, data = iris)
		),
		expected = c("iris$Sepal.Width")
	),
	# Test argument with log
	call.argument.2 = list(
		x = substitute(
			glm(Petal.Length ~ ., offset = log(Sepal.Width), data = iris)
		),
		expected = c("Sepal.Width")
	),
	# Test argument with log and '$'
	call.argument.3 = list(
		x = substitute(
			glm(Petal.Length ~ ., offset = log(iris$Sepal.Width), data = iris)
		),
		expected = c("iris$Sepal.Width")
	),
	# Test argument and formula together.
	call.both = list(
		x = substitute(
			glm(
				Petal.Length ~ . + offset(log(Petal.Width)),
				offset = Sepal.Width, data = iris
			)
		),
		expected = c("Petal.Width", "Sepal.Width")
	)
)

for (i in names(test.data)) {
	test.data[[gsub("^call", "object", i)]] <- test.data[[i]]
	test.data[[i]]$x <- eval(test.data[[i]]$x)
}


#------------------------------------------------------------------------------
#	Run tests.
#------------------------------------------------------------------------------
test_that(
	"Testing model.interface.derault$has.offset()",
	{
		for (i in names(test.data)) {
			expect_equal(
				model.adapter:::model.interface.default()$get.offset.names(
					test.data[[i]]$x, .GlobalEnv, "stats"
				),
				test.data[[i]]$expected, info = sprintf("While testing %s", i)
			)
		}
	}
)

