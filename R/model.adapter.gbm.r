#-------------------------------------------------------------------------------
#'	model.adapter class for gbm
#'
#'	This reference class contains methods for \code{\link[gbm]{gbm}} in 
#'	\emph{gbm} package.
#'	Following methods are overriden.
#-------------------------------------------------------------------------------
#	gbm�֐��p��model.adapter�I�u�W�F�N�g�̃W�F�l���[�^�[�B
#-------------------------------------------------------------------------------
model.adapter.gbm <- setRefClass(
	"model.adapter.gbm", contains = "model.adapter"
)

