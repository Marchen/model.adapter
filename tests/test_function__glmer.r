# Test for glmer
test__all(
	call = substitute(
		glmer(Sepal.Length ~ . + (1 | Species), data = iris, family = Gamma)
	),
	function.name = "glmer",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species + (1 | Species),
	family = "Gamma",
	data = iris
)

