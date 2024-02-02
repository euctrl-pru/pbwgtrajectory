#' Title
#'
#' @param .df 
#'
#' @return
#' @export
#'
#' @examples
coerce_unixepoch_to_datetime <- function(.df, .time_var=TIME){
  .df |> dplyr::mutate(
    {{.time_var}} := lubridate::as_datetime({{.time_var}})
  )
}