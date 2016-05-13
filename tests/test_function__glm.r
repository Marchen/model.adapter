# Test for glm
test__all(
	call = substitute(glm(Sepal.Length ~ ., data = iris, family = gaussian)),
	function.name = "glm",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
	family = "gaussian"
)

