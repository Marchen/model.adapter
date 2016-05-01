# Test for gam in gam package.
test__all(
	substitute(gam(Sepal.Length ~ Petal.Length, data = iris)), "gam", "gam"
)
