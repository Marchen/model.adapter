# Test for glmer
test__all(
	substitute(
		glmer(Sepal.Length ~ . + (1 | Species), data = iris, family = Gamma)
	),
	"glmer"
)

