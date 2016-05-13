# Test for gam in mgcv package.
test__all(
	call = substitute(
		gam(Sepal.Length ~ Petal.Length, data = iris, family = gaussian)
	),
	function.name = "gam",
	formula = Sepal.Length ~ Petal.Length,
	package.name = "mgcv",
	family = "gaussian",
	data = iris
)

