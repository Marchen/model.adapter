# Test for tree
test__all(
	call = substitute(tree(Sepal.Length ~ ., data = iris)),
	function.name = "tree",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
	data = iris
)
