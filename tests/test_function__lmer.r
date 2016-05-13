# Test for lmer
test__all(
	call = substitute(lmer(Sepal.Length ~ . + (1 | Species), data = iris)),
	function.name = "lmer",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species + (1 | Species),
	data = iris
)
