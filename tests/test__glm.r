# Test for glm
test__all(
	substitute(glm(Sepal.Length ~ ., data = iris)), "glm"
)
