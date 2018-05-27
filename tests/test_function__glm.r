#==============================================================================
#	Test for glm
#==============================================================================

test <- glm.type.test.runner$new(
	"glm",
	families = c(
		"gaussian", "Gamma", "inverse.gaussian", "poisson", "binomial"
	)
)
test$run()
rm(test)
