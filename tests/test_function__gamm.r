#==============================================================================
#	Test for gamm in mgcv package.
#==============================================================================

test.gamm <- ma.test(
	call = gamm(
		Sepal.Length ~ s(Petal.Length), data = iris, family = gaussian
	),
	function.name = "gamm",
	formula = Sepal.Length ~ s(Petal.Length),
	object.has.call = FALSE,
	family = "gaussian",
	data = iris
)
test.gamm$run.all()
rm(test.gamm)
