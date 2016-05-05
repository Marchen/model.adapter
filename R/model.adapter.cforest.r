#-------------------------------------------------------------------------------
#'	model.adapter class for cforest
#'
#'	This reference class contains methods for \code{\link[party]{cforest}} in 
#'	\emph{party} package.
#'	Note that because an object of RandomForest does not keep original call,
#'	get.call() function always returns NULL. Also, when an instance of this 
#'	class is made from model object, 'call' field is always call("<undef>").
#'
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	cforest関数用のmodel.adapterオブジェクトのジェネレーター。
#-------------------------------------------------------------------------------
model.adapter.RandomForest <- setRefClass(
	"model.adapter.RandomForest", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	モデルオブジェクトからcallを取得する。
#-------------------------------------------------------------------------------
model.adapter.RandomForest$methods(
	get.call = function(x) {
		return(NULL)
	}
)

#-------------------------------------------------------------------------------
#	formulaを取り出し。
#-------------------------------------------------------------------------------
model.adapter.RandomForest$methods(
	get.formula = function(x) {
		if (is.object(x)) {
			# custom 
			y <- as.character(x@data@formula$response[2])
			x <- as.character(x@data@formula$input[2])
			f <- as.formula(paste(y, x, sep = "~"))
			return(f)
		} else {
			x <- match.call(cforest, x)
			return(eval(x$formula))
		}
	}
)

