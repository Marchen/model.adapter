#==============================================================================
#	Test for lme
#==============================================================================

test.lme <- ma.test(
	call = lme(Sepal.Length ~ ., random = ~1 | Species, data = iris),
	function.name = "lme",
	formula = Sepal.Length
		~ Sepal.Width + Petal.Length + Petal.Width + Species,
	data = iris
)
test.lme$run.all()
rm(test.lme)
