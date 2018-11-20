#==============================================================================
#	Test for gam in gam/mgcv package.
#==============================================================================
test <- glm.type.test.runner$new(
	"gam", package = "gam",
	families = c(
		"gaussian", "Gamma", "inverse.gaussian", "poisson", "binomial"
	)
)
test$run()
rm(test)
