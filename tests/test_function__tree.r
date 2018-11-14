#==============================================================================
#	Test for tree
#==============================================================================
test.data <- list(
	call = list(
		substitute(tree(Sepal.Length ~ ., data = iris)),
		substitute(tree(Species ~ ., data = iris))
	),
	formula = list(
		Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
		Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
	),
	model.type = list("regression", "classification")
)

test.model.adapter("tree", iris, test.data)

rm(test.data)
