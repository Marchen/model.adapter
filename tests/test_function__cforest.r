# Test for cforest
test__all(
	call = substitute(cforest(Sepal.Length ~ ., data = iris)),
	function.name = "cforest",
	formula = Sepal.Length ~ .,
	object.has.call = FALSE
)
