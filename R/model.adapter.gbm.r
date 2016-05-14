#-------------------------------------------------------------------------------
#	gbm関数用のmodel.adapterオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
#'	model.adapter class for gbm
#'
#'	This reference class contains methods for \code{\link[gbm]{gbm}} in 
#'	\emph{gbm} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.adapter.default.r
#'	@family model.adapter
#'	@export model.adapter.gbm
#'	@exportClass model.adapter.gbm
#-------------------------------------------------------------------------------
model.adapter.gbm <- setRefClass(
	"model.adapter.gbm", contains = "model.adapter"
)


#-------------------------------------------------------------------------------
#	モデル作成に使われたデータを返す。
#-------------------------------------------------------------------------------
model.adapter.gbm$methods(
	get.data = function(x, envir = parent.frame()) {
		# Extract data from call using default method.
		# If default method couldn't get data, try to use data in call 
		# in the object.
		# デフォルトメソッドでcallからdataを取得。
		# デフォルトのメソッドでdataを取得できなかったら、
		# x$callからデータを取り出す。
		if (is.call(x)) {
			d <- callSuper(x, envir)
		} else {
			model.call <- match.generic.call(.self$get.call(x))
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


