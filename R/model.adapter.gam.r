#-------------------------------------------------------------------------------
#'	model.adapter class for gam
#'
#'	This reference class contains methods for \code{\link[mgcv]{gam}} in 
#'	\emph{mgcv} package and \code{\link[gam]{gam}} in \emph{gam} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	gam�֐��p��model.adapter�I�u�W�F�N�g�̃W�F�l���[�^�[�B
#-------------------------------------------------------------------------------
model.adapter.gam <- setRefClass(
	"model.adapter.gam", contains = "model.adapter"
)


