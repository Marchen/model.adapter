# Test for gamm in mgcv package.
test__all(
	substitute(gamm(Sepal.Length ~ s(Petal.Length), data = iris)), "gamm"
)
