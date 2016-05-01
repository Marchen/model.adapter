# Test for cforest
test__all(
	substitute(cforest(Sepal.Length ~ ., data = iris)), "cforest"
)
