# Test for glmer
test__all(
	call = substitute(
		glmer(Sepal.Length ~ . + (1 | Species), data = iris, family = Gamma)
	),
	function.name = "glmer",
	family = "Gamma"
)

