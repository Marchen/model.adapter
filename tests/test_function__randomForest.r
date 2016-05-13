# Test for randomForest
test__all(
	call = substitute(randomForest(Sepal.Length ~ ., data = iris)),
	function.name = "randomForest",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
	data = iris
)
