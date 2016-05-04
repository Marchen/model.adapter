# Test for cforest
test__all(
	call = substitute(cforest(Sepal.Length ~ ., data = iris)),
	function.name = "cforest",
	object.has.call = FALSE
)
