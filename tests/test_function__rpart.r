# Test for rpart
test__all(
	call = substitute(rpart(Sepal.Length ~ ., data = iris)),
	function.name = "rpart",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species
)
