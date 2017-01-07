#==============================================================================
#	Test for tree
#==============================================================================

test.tree <- ma.test(
	call = tree(Sepal.Length ~ ., data = iris),
	function.name = "tree",
	formula = Sepal.Length
		~ Sepal.Width + Petal.Length + Petal.Width + Species,
	data = iris
)
test.tree$run.all()
rm(test.tree)
