#------------------------------------------------------------------------------
#	tree関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
#'	model.interface class for tree
#'
#'	This reference class contains methods for \code{\link[tree]{tree}} in
#'	\emph{tree} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.tree
#'	@exportClass model.interface.tree
#------------------------------------------------------------------------------
model.interface.tree <- setRefClass(
	"model.interface.tree", contains = "model.interface"
)


#------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#------------------------------------------------------------------------------
model.interface.tree$methods(
	predict.types = function() {
		type <- make.predict.types(
			response = "vector", link = "vector", prob = "vector"
		)
		return(type)
	}
)


