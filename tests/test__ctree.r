# Test for ctree
test__all(
	substitute(ctree(Sepal.Length ~ ., data = iris)), "ctree"
)
