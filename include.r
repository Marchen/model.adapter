#-------------------------------------------------------------------------------
#	ƒeƒXƒg—p‚É‘S‚Ä‚ÌƒXƒNƒŠƒvƒg‚ğ“Ç‚İ‚ŞB
#	Read all scripts for developmental purpose.
#-------------------------------------------------------------------------------
#	function		class			package
#	-----------------------------------------------
#	lm				lm				stats
#	glm				glm, lm			stats
#	lme				lme				nlme
#	glmmML			glmmML			glmmML
#	lmer			lmerMod			lme4
#	glmer			glmerMod		lme4
#	ctree			BinaryTree		party
#	cforest			RandomForest	party
#	randomForest	randomForest	randomForest
#	gbm				gbm				gbm
#	svm				svm.formula		e1071
#	tree			tree			tree
#	rpart			rpart			rpart
#	gam				gam				mgcv, gam
#	gamm			gamm			mgcv
#	ranger			ranger			ranger
#	MCMCglmm		MCMCglmm		MCMCglmm
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#	List of type of predict methods.
#-------------------------------------------------------------------------------
#	function		response	link		prob	class		remarks
#	----------------------------------------------------------------------------
#	cforest			response	----		prob	response	ï¿½ï¿½ï¿½ï¿½ï¿½Ïï¿½ï¿½^ï¿½Å”ï¿½ï¿½ï¿½
#	ctree			response	----		prob	response	ï¿½ï¿½ï¿½ï¿½ï¿½Ïï¿½ï¿½^ï¿½Å”ï¿½ï¿½ï¿½
#	gam				response	link		----	----
#	gamm			response	link		----	----
#	gbm				response	link		----	----
#	glm				response	link		----	----
#	glmer			response	link		----	----
#	glmmadmb		response	link		----	----
#	glmmML			response	link		----	----
#	lm				response	----		----	----
#	lme				----		----		----	----
#	lmer			response	link		----	----
#	MCMCglmm		response	term		----	----
#	randomForest	response	----		prob	response	ï¿½ï¿½ï¿½ï¿½ï¿½Ïï¿½ï¿½^ï¿½Å”ï¿½ï¿½ï¿½
#	ranger			----		----		----	----		ï¿½ï¿½ï¿½Ê‚É‚ï¿½ï¿½ï¿½ï¿½Èï¿½ñ‚ª“ï¿½ï¿½ï¿½ï¿½Ä‚ï¿½B
#	rpart			vector		matrix?		prob	class		matrixï¿½ï¿½linkï¿½ï¿½ï¿½ï¿½ï¿½ï¿½Å‚ï¿½ï¿½ï¿½Ì‚ï¿½ï¿½H
#	svm				----		----		----	----		probabilityï¿½ï¿½ï¿½ï¿½ï¿½ÅƒRï¿½ï¿½ï¿½gï¿½ï¿½ï¿½[ï¿½ï¿½
#	tree			vector		----		vector	class
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#	ƒXƒNƒŠƒvƒg‚ª‚ ‚éƒfƒBƒŒƒNƒgƒŠ–¼‚ğ•Ô‚·ŠÖ”B
#	http://stackoverflow.com/questions/1815606/rscript-determine-path-of-the-executing-script
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

#-------------------------------------------------------------------------------
#	ƒ\[ƒX“Ç‚İ‚İ
#-------------------------------------------------------------------------------
base.path <- file.path(get.this.file.dir(), "R")
source(file.path(base.path, "model.interface.default.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.cforest.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.ctree.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.gam.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.gamm.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.gbm.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.glm.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.glmer.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.glmmML.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.lm.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.lme.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.lmer.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.MCMCglmm.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.randomForest.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.ranger.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.rpart.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.svm.r"), encoding = "UTF-8")
source(file.path(base.path, "model.interface.tree.r"), encoding = "UTF-8")
source(file.path(base.path, "model.adapter.r"), encoding = "UTF-8")

source(file.path(base.path, "utils.r"), encoding = "UTF-8")
source(file.path(base.path, "const.r"), encoding = "UTF-8")
source(file.path(base.path, "find.package.r"), encoding = "UTF-8")
source(file.path(base.path, "get.class.name.r"), encoding = "UTF-8")
source(file.path(base.path, "ma.prediction.r"), encoding = "UTF-8")
source(file.path(base.path, "predict.types.r"), encoding = "UTF-8")

source(file.path(base.path, "tests.r"), encoding = "UTF-8")
