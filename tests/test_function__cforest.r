# Test for cforest
test__all(
	call = substitute(
		cforest(
			Sepal.Length ~ ., data = iris, 
			controls = cforest_control(ntree = 10)
		)
	),
	function.name = "cforest",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
	object.has.call = FALSE,
	data = iris
)
