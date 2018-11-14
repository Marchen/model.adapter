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
model.interface.tree$methods(
	predict.types = function() {
		type <- make.predict.types(
			response = "vector", link = "vector", prob = "vector"
		)
		return(type)
	}
)
