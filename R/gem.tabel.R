#' Gem tabel
#'
#' Gemmer en tabel i undermappe med navn tabeller
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

gem.tabel <- function(x){
  # ud <- kable(, format.args = list(big.mark=".", decimal.mark=","), digits = 2, format = "markdown")
  navnet2 <- deparse(substitute(x))
  write_excel_csv2(x,paste0("./tabeller/",format(Sys.time(), "%d.%b%y"),"_",navnet2,"_",".csv"))
  print(x)
}
