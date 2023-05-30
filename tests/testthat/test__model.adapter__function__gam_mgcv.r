#==============================================================================
#   Test for gam in mgcv package.
#==============================================================================

source("tests.r")
test <- glm.type.test.runner$new(
    "gam", package = "mgcv",
    families = c(
        "gaussian", "Gamma", "inverse.gaussian", "poisson", "binomial"
    )
)
test$run()
rm(test)
