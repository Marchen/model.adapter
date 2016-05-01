# Test for tree
test__all(
	call = substitute(tree(Sepal.Length ~ ., data = iris)),
	function.name = "tree"
)
