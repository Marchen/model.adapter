# Test for ranger
test__all(
	call = substitute(ranger(Sepal.Length ~ ., data = iris)),
	function.name = "ranger",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
	data = iris
)
