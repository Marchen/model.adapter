#==============================================================================
#	Test for gam in mgcv package.
#==============================================================================
test <- glm.type.test.runner$new(
	"gam", package = "mgcv",
	families = c(
		"gaussian", "Gamma", "inverse.gaussian", "poisson", "binomial"
	)
)
test$run()
rm(test)