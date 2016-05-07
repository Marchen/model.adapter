# Test for lm
test__all(
	call = substitute(lm(Sepal.Length ~ ., data = iris)),
	function.name = "lm",
	formula = Sepal.Length ~ .
)
