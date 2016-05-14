#-------------------------------------------------------------------------------
#	Run all tests of functions.
#-------------------------------------------------------------------------------

get.this.file.dir <- function() {
	cmdArgs <- commandArgs(trailingOnly = FALSE)
	needle <- "--file="
	match <- grep(needle, cmdArgs)
	if (length(match) > 0) {
		# Rscript
		return(dirname(sub(needle, "", cmdArgs[match])))
	} else {
		# 'source'd via R console
		return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
	}
}

base.path <- get.this.file.dir()

source(file.path(base.path, "test_function__cforest.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__ctree.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__gam_gam.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__gam_mgcv.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__gamm.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__gbm.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__glm.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__glmer.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__glmmML.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__lm.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__lme.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__lmer.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__MCMCglmm.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__randomForest.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__ranger.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__rpart.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__svm.r"), encoding = "UTF-8")
source(file.path(base.path, "test_function__tree.r"), encoding = "UTF-8")




