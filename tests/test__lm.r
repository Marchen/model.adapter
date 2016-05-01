# Test for lm
test__all(
	substitute(lm(Sepal.Length ~ ., data = iris)), "lm"
)
