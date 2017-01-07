#==============================================================================
#	Test for glmmML
#==============================================================================

test.glmmML <- ma.test(
	call = glmmML(
		as.integer(Sepal.Length) ~ ., data = iris, family = poisson,
		cluster = Species
	),
	function.name = "glmmML",
	formula = as.integer(Sepal.Length)
		~ Sepal.Width + Petal.Length + Petal.Width + Species,
	family = "poisson",
	data = iris
)
test.glmmML$run.all()
rm(test.glmmML)

