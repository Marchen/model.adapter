#==============================================================================
#	Test for glmer
#==============================================================================
test.glmer <- ma.test(
	call = glmer(
		Sepal.Length ~ . + (1 | Species), data = iris, family = Gamma
	),
	function.name = "glmer",
	formula = Sepal.Length
		~ Sepal.Width + Petal.Length + Petal.Width + Species + (1 | Species),
	family = "Gamma",
	data = iris
)

test.glmer$run.all()
rm(test.glmer)

