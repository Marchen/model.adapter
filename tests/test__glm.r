# Test for glm
test__all(
	call = substitute(glm(Sepal.Length ~ ., data = iris, family = gaussian)),
	function.name = "glm",
	family = "gaussian"
)

