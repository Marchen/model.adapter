# Test for randomForest
test__all(
	call = substitute(randomForest(Sepal.Length ~ ., data = iris)),
	function.name = "randomForest",
	formula = Sepal.Length ~ .
)
