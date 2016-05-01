# Test for lmer
test__all(
	substitute(lmer(Sepal.Length ~ . + 1|Species, data = iris)), "lmer"
)
