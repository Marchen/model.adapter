#==============================================================================
#	Test for svm
#==============================================================================
test.data <- list(
	call = list(
		substitute(svm(Sepal.Length ~ ., data = iris)),
		substitute(svm(Species ~ ., data = iris))
	),
	formula = list(
		Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
		Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
	),
	model.type = list("regression", "classification")
)

test.model.adapter("svm", iris, test.data)

rm(test.data)