#------------------------------------------------------------------------------
#	svm関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
#'	model.interface class for svm
#'
#'	This reference class contains methods for \code{\link[e1071]{svm}} in
#'	\emph{e1071} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.svm
#'	@exportClass model.interface.svm
#------------------------------------------------------------------------------
model.interface.svm <- setRefClass(
	"model.interface.svm", contains = "model.interface"
)


#------------------------------------------------------------------------------
#	svm用predict()メソッド。
#------------------------------------------------------------------------------
model.interface.svm$methods(
	predict = function(object, newdata = NULL, type, ...) {
		if (is.null(newdata)) {
			pred <- stats::predict(
				object, predict.all = TRUE, probability = TRUE, ...
			)
		} else {
			pred <- stats::predict(
				object, newdata = newdata, predict.all = TRUE,
				probability = TRUE, ...
			)
		}
		if (type == "prob") {
			# If type is "prob", extract probability.
			pred <- attr(pred, "probabilities")
		}
		return(pred)
	}
)

