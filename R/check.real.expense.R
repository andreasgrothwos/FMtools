#' Tjek om udgiften er reelt forbrug
#'
#' Returnerer sand/falsk om tallet går lige op i 50.
#' @param listen Liste med tal
#' @keywords forbrug
#' @export
#' @examples
#' cat_function()

check.real.expense <- function(listen) {
	tjek <- listen %/% 50
	tjek <- ifelse (tjek == 0, 0.11, tjek)

	real.expense <- (listen - tjek * 50 != 0)
}
