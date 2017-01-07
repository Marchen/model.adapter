#==============================================================================
#	Test for glm
#==============================================================================

# Prepare test object.
test.glm <- ma.test(
	call = glm(Sepal.Length ~ ., data = iris, family = gaussian),
	function.name = "glm",
	formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
	family = "gaussian", data = iris
)


# Prepare test for link/linkinv functions.
iris2 <- iris
iris2$bin <- ifelse(iris2$Species == "setosa", 1, 0)

test.glm$register.link.test.data(
	glm(Sepal.Length ~ ., data = iris2, family = gaussian),
	gaussian()$linkfun, gaussian()$linkinv
)

test.glm$register.link.test.data(
	glm(Sepal.Length ~ ., data = iris2, family = "gaussian"),
	gaussian()$linkfun, gaussian()$linkinv
)

test.glm$register.link.test.data(
	glm(Sepal.Length ~ ., data = iris2, family = gaussian(log)),
	gaussian(log)$linkfun, gaussian(log)$linkinv
)

test.glm$register.link.test.data(
	glm(Sepal.Length ~ ., data = iris2, family = gaussian("log")),
	gaussian("log")$linkfun, gaussian("log")$linkinv
)

test.glm$register.link.test.data(
	glm(Sepal.Length ~ ., data = iris2, family = poisson),
	poisson()$linkfun, poisson()$linkinv
)

test.glm$register.link.test.data(
	glm(bin ~ ., data = iris2, family = binomial),
	binomial()$linkfun, binomial()$linkinv
)

test.glm$register.link.test.data(
	glm(Sepal.Length ~ ., data = iris2, family = Gamma),
	Gamma()$linkfun, Gamma()$linkinv
)

test.glm$register.link.test.data(
	glm(Sepal.Length ~ ., data = iris2, family = inverse.gaussian),
	inverse.gaussian()$linkfun, inverse.gaussian()$linkinv
)

test.glm$register.link.test.data(
	glm(Sepal.Length ~ ., data = iris2, family = quasi),
	quasi()$linkfun, quasi()$linkinv
)

test.glm$register.link.test.data(
	glm(bin ~ ., data = iris2, family = quasibinomial),
	quasibinomial()$linkfun, quasibinomial()$linkinv
)

test.glm$register.link.test.data(
	glm(Sepal.Length ~ ., data = iris2, family = quasipoisson),
	quasipoisson()$linkfun, quasipoisson()$linkinv
)


# Run test.
test.glm$run.all()
rm(iris2, test.glm)

