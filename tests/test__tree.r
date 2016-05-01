# Test for tree
test__all(
	substitute(tree(Sepal.Length ~ ., data = iris)), "tree"
)
