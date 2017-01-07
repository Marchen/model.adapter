#------------------------------------------------------------------------------
#	lm関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
#'	model.interface class for lm
#'
#'	This reference class contains methods for \code{\link[stats]{lm}} in
#'	\emph{stats} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.lm
#'	@exportClass model.interface.lm
#------------------------------------------------------------------------------
model.interface.lm <- setRefClass(
	"model.interface.lm", contains = "model.interface"
)


#------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#------------------------------------------------------------------------------
model.interface.lm$methods(
	predict.types = function() {
		types <- make.predict.types(
			link = "response", prob = "response", class = "response"
		)
		return(types)
	}
)



