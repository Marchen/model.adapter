#==============================================================================
#	Test for randomForest
#==============================================================================
test.data <- list(
	call = list(
		substitute(randomForest(Sepal.Length ~ ., data = iris)),
		substitute(randomForest(Species ~ ., data = iris))
	),
	formula = list(
		Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
		Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
	),
	model.type = list("regression", "classification")
)

test.model.adapter("randomForest", iris, test.data)

rm(test.data)
