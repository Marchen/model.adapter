#------------------------------------------------------------------------------
#	gam関数用のmodel.interfaceオブジェクトのジェネレーター。
#	以下のメソッドをオーバーライドした。
#------------------------------------------------------------------------------
#'	model.interface class for gam
#'
#'	This reference class contains methods for \code{\link[mgcv]{gam}} in
#'	\emph{mgcv} package and \code{\link[gam]{gam}} in \emph{gam} package.
#'
#'	Following methods are overriden.
#'
#'	@include model.interface.default.r
#'	@family model.interface
#'	@export model.interface.gam
#'	@exportClass model.interface.gam
#------------------------------------------------------------------------------

# gam in mgcv package.
model.interface.gam <- setRefClass(
	"model.interface.gam", contains = "model.interface"
)

# gam in gam package.
model.interface.Gam <- setRefClass(
	"model.interface.Gam", contains = "model.interface"
)



#------------------------------------------------------------------------------
#	formulaの.を展開する。
#------------------------------------------------------------------------------

# mgcv
model.interface.gam$methods(
	expand.formula = function(f, d, specials = NULL, package = "mgcv") {
		return(callSuper(f, d, specials = c("s", "te", "ti", "t2")))
	}
)

# gam
model.interface.Gam$methods(
	expand.formula = function(f, d, specials = NULL, package = "mgcv") {
		require(gam)
		return(callSuper(f, d, specials = gam::gam.slist))
	}
)


#------------------------------------------------------------------------------
#	predictのtypeを関数に合わせて変換する変換表を取得する。
#------------------------------------------------------------------------------

# mgcv
model.interface.gam$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)

# gam
model.interface.Gam$methods(
	predict.types = function() {
		return(make.predict.types(prob = "response", class = "response"))
	}
)
