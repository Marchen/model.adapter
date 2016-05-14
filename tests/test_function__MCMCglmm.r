# Test for MCMCglmm
test__all(
	call = substitute(
		MCMCglmm(
			Sepal.Length ~ Petal.Length, data = iris, family = "gaussian",
			verbose = FALSE
		)
	),
	function.name = "MCMCglmm",
	formula = Sepal.Length ~ Petal.Length,
	object.has.call = FALSE,
	object.has.data = FALSE,
	family = "gaussian",
	data = iris
)
