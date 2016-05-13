# Test for gbm.
test__all(
	call = substitute(gbm(Sepal.Length ~ Petal.Length, data = iris)),
	function.name = "gbm",
	formula = Sepal.Length ~ Petal.Length,
	data = iris
)
