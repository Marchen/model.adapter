# Test for ctree
test__all(
	call = substitute(ctree(Sepal.Length ~ ., data = iris)),
	function.name = "ctree"
)