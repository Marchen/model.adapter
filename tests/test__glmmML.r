# Test for glmmML
test__all(
	substitute(
		glmmML(
			as.integer(Sepal.Length) ~ ., data = iris, family = poisson,
			cluster = Species
		)
	),
	"glmmML"
)


