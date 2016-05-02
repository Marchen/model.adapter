# Test for ranger
test__all(
	call = substitute(ranger(Sepal.Length ~ ., data = iris)),
	function.name = "ranger"
)
