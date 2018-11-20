source("tests.r")

cat("Testing stepAIC...\n")
# Prepare objects and calls for testing.
calls <- list(
	glm = substitute(
		glm(
			Sepal.Length ~ Species + Petal.Length,
			data = iris, family = gaussian
		)
	),
	lm = substitute(lm(Sepal.Length ~ Species + Petal.Length, data = iris)),
	lme = substitute(
		lme(
			Sepal.Length ~ Species + Petal.Length, random = ~1 | Species,
			data = iris, method = "ML"
		)
	)
)

objects <- lapply(calls, eval)

# do MASS::stepAIC.
result.stepAIC <- lapply(objects, MASS::stepAIC, trace = FALSE)

# Test result of stepAIC does not produce errors.
expect_silent(
	null <- lapply(result.stepAIC, model.adapter$new)
)

# Remove objects.
rm(calls, objects, result.stepAIC, null)
