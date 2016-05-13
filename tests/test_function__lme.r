# Test for lme
test__all(
	call = substitute(
		lme(Sepal.Length ~ ., random = ~1 | Species, data = iris)
	),
	function.name = "lme",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species
)
