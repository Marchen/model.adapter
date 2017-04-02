#==============================================================================
#	Test for glm
#==============================================================================

test <- glm.type.test.runnner(
	"glm",
	families = c(
		"gaussian", "Gamma", "inverse.gaussian", "poisson", "binomial"
	)
)
test$run()
rm(test)
