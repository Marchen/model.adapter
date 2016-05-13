# Test for lm
test__all(
	call = substitute(lm(Sepal.Length ~ ., data = iris)),
	function.name = "lm",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species
)
