#==============================================================================
#	Test for glm
#==============================================================================

source("tests.r")
test <- glm.type.test.runner$new(
	"glm",
	families = c(
		"gaussian", "Gamma", "inverse.gaussian", "poisson", "binomial"
	)
)
test$run()
rm(test)
