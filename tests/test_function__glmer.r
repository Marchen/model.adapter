# Test for glmer
test__all(
	call = substitute(
		glmer(Sepal.Length ~ . + (1 | Species), data = iris, family = Gamma)
	),
	function.name = "glmer",
	formula = Sepal.Length ~ . + (1 | Species),
	family = "Gamma"
)

