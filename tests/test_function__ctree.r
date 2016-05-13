# Test for ctree
test__all(
	call = substitute(ctree(Sepal.Length ~ ., data = iris)),
	function.name = "ctree",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
	object.has.call = FALSE
)
