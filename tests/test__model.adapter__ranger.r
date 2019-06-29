#==============================================================================
#	Test for ranger
#==============================================================================

source("tests.r")
iris2 <- iris
iris2$Species <- as.character(iris2$Species)

test.data <- list(
	call = list(
		substitute(
			ranger(Sepal.Length ~ ., data = iris, write.forest = TRUE)
		),
		substitute(ranger(Species ~ ., data = iris, write.forest = TRUE)),
		substitute(
			ranger(
				Species ~ ., data = iris2, write.forest = TRUE,
				probability = TRUE
			)
		)
	),
	formula = list(
		Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
		Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
		Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
	),
	model.type = list("regression", "classification", "classification")
)

test.model.adapter("ranger", iris, test.data)

rm(test.data)
