#------------------------------------------------------------------------------
#' (Internal) Prepare predict.types.
#'
#' Prepare a named character vector for predict.types.
#'
#' @param predict.types a character vector of predict.types.
#' @param response a character representing predict type of reponse scale.
#' @param link a character representing predict type of link scale.
#' @param prob a character representing predict type of class probability.
#' @param class a character representing predict type of class labels.
#'
#' @return a character representing predict.types.
#------------------------------------------------------------------------------
#	predict.typesに使う文字列ベクトルを作成する。
#
#	Args:
#		predict.types: predict.typesに使う文字列。
#		reponse: 応答変数のスケールでの予測値を計算させる時のtype。
#		link: リンク関数のスケールでの予測値を計算させる時のtype。
#		prob: 各クラスの確率を計算させる時のtype。
#		class: 予測されるクラスのラベルを計算させる時のtype。
#
#	Value:
#		predict.typesに使う文字列。
#------------------------------------------------------------------------------
make.predict.types <- function(
	predict.types,
	response = "response", link = "link", prob = "prob", class = "class"
) {
	if (missing(predict.types)) {
		predict.types <- c(response, link, prob, class)
	}
	names(predict.types) <- c("response", "link", "prob", "class")
	return(predict.types)
}
