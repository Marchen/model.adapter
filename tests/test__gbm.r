# Test for gbm.
test__all(
	substitute(gbm(Sepal.Length ~ Petal.Length, data = iris)), "gbm"
)
