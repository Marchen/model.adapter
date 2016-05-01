# Test for ranger
test__all(
	substitute(ranger(Sepal.Length ~ ., data = iris)), "ranger"
)
