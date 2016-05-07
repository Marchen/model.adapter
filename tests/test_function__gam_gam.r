# Test for gam in gam package.
test__all(
	call = substitute(
		gam(Sepal.Length ~ Petal.Length, data = iris, family = gaussian)
	),
	function.name = "gam",
	formula = Sepal.Length ~ Petal.Length,
	package.name = "gam",
	family = "gaussian"
)

