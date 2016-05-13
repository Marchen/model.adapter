# Test for cforest
test__all(
	call = substitute(cforest(Sepal.Length ~ ., data = iris)),
	function.name = "cforest",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
	object.has.call = FALSE
)
