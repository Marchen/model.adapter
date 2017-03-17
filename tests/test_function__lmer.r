#==============================================================================
#	Test for lmer
#==============================================================================

test.lmer <- ma.test(
	call = lmer(Sepal.Length ~ . + (1 | Species), data = iris),
	function.name = "lmer",
	formula = Sepal.Length
		~ Sepal.Width + Petal.Length + Petal.Width + Species + (1 | Species),
	data = iris, family = "gaussian"
)
test.lmer$run.all()
rm(test.lmer)
