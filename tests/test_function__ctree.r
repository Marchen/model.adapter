#==============================================================================
#	Test for ctree
#==============================================================================

test.ctree <- ma.test(
	call = ctree(Sepal.Length ~ ., data = iris),
	function.name = "ctree",
	formula = Sepal.Length
		~ Sepal.Width + Petal.Length + Petal.Width + Species,
	object.has.call = FALSE,
	data = iris
)
test.ctree$run.all()
rm(test.ctree)
