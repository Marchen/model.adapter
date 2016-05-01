# Test for lme
test__all(
	substitute(lme(Sepal.Length ~ ., random=~1|Species, data = iris)), "lme"
)
