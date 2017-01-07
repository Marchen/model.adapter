#==============================================================================
#	Test for gam in gam package.
#==============================================================================

test.gam <- ma.test(
	call = gam(Sepal.Length ~ Petal.Length, data = iris, family = gaussian),
	function.name = "gam",
	formula = Sepal.Length ~ Petal.Length,
	package.name = "gam",
	family = "gaussian",
	data = iris
)

test.gam$run.all()
rm(test.gam)
