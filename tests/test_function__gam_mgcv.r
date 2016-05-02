# Test for gam in mgcv package.
test__all(
	call = substitute(
		gam(Sepal.Length ~ Petal.Length, data = iris, family = gaussian)
	),
	function.name = "gam",
	package.name = "mgcv",
	family = "gaussian"
)

