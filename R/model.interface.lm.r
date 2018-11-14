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
model.interface.lm$methods(
	get.family = function(x, type = c("character", "family"), envir) {
		return(format.family("gaussian", type))
	}
)


#------------------------------------------------------------------------------
model.interface.lm$methods(
	predict.types = function() {
		types <- make.predict.types(
			link = "response", prob = "response", class = "response"
		)
		return(types)
	}
)
