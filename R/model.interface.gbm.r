#-------------------------------------------------------------------------------
#	gbm関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#-------------------------------------------------------------------------------
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
#-------------------------------------------------------------------------------
model.interface.gbm <- setRefClass(
	"model.interface.gbm", contains = "model.interface"
)


#-------------------------------------------------------------------------------
#	モデル作成に使われたデータを返す。
#-------------------------------------------------------------------------------
model.interface.gbm$methods(
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


