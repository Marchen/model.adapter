# Test for gamm in mgcv package.
test__all(
	call = substitute(
		gamm(Sepal.Length ~ s(Petal.Length), data = iris, family = gaussian)
	),
	function.name = "gamm",
	family = "gaussian"
)

