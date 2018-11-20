#==============================================================================
#	Test for ranger
#==============================================================================

source("tests.r")

test.data <- list(
	call = list(
		substitute(
			ranger(Sepal.Length ~ ., data = iris, write.forest = TRUE)
		),
		substitute(ranger(Species ~ ., data = iris, write.forest = TRUE))
	),
	formula = list(
		Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
		Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
	),
	model.type = list("regression", "classification")
)

test.model.adapter("ranger", iris, test.data)

rm(test.data)
