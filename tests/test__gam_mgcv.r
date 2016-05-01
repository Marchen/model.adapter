# Test for gam in mgcv package.
test__all(
	substitute(gam(Sepal.Length ~ Petal.Length, data = iris)), "gam", "mgcv"
)
