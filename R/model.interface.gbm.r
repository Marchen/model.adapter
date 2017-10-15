#------------------------------------------------------------------------------
#	gbm関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
#'	model.interface class for gbm
#'
#'	This reference class contains methods for \code{\link[gbm]{gbm}} in
#'	\emph{gbm} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.gbm
#'	@exportClass model.interface.gbm
#------------------------------------------------------------------------------
model.interface.gbm <- setRefClass(
	"model.interface.gbm", contains = "model.interface"
)


#------------------------------------------------------------------------------
#	モデル作成に使われたデータを返す。
#------------------------------------------------------------------------------
model.interface.gbm$methods(
	get.data = function(x, envir = parent.frame(), package = "") {
		# Extract data from call using default method.
		# If default method couldn't get data, try to use data in call
		# in the object.
		# デフォルトメソッドでcallからdataを取得。
		# デフォルトのメソッドでdataを取得できなかったら、
		# x$callからデータを取り出す。
		if (is.call(x)) {
			d <- callSuper(x, envir)
		} else {
			model.call <- match.generic.call(.self$get.call(x), envir, package)
			d <- eval(model.call$data)
		}
		# If still couldn't get data, use data in the object and make a
		# a data.frame manually.
		# それでもデータをとれなかったらオブジェクトのdataからデータを作成。
		if (is.null(d)){
			warning("Making data.frame using data field in gbm object.")
			y.var <- x$data$y
			colnames(y.var) <- x$response.name
			x.vars <- x$data$x
			colnames(x.vars) <- x$var.names
			d <- as.data.frame(cbind(y.var, x.var))
			rownames(d) <- NULL
		}
		return(d)
	}
)


#------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#------------------------------------------------------------------------------
model.interface.gbm$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)
#------------------------------------------------------------------------------
#	予測値を計算する。
#------------------------------------------------------------------------------
model.interface.gbm$methods(
	predict = function(object, newdata, type, random, ...) {
		pred <- callSuper(object, newdata, type, random, ...)
		if (is.array(pred)) {
			pred <- as.matrix(pred[, , 1])
		}
		# If the prediction type is "class", convert the result to class label.
		if (names(type) == "class") {
			pred <- colnames(pred)[apply(pred, 1, which.max)]
		}
		return(pred)
	}
)


#------------------------------------------------------------------------------
#	モデルの種類を返す。
#------------------------------------------------------------------------------
model.interface.gbm$methods(
	get.model.type = function(x, envir = parent.frame(), package = "", ...) {
		"
		return a character vector specifying model type
		(regression or classification).
		"
		if (is.call(x)) {
			distribution <- x$distribution
			if (is.null(distribution)) {
				y.name <- as.character(.self$get.formula(x, envir)[2])
				data <- .self$get.data(x, envir, package)
				distribution <- gbm::guessDist(data[[y.name]])$name
			}
		} else {
			distribution <- x$distribution$name
		}
		classification.families <- c(
			"bernoulli", "huberized", "multinomial", "adaboost"
		)
		if (distribution %in% classification.families){
			return("classification")
		} else {
			return("regression")
		}
	}
)

