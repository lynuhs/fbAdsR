#' Collect data from Facebook API
#'
#' This function will import the data for the chosen parameters.
#' @param object The object to select data from.
#' @param scope The scope to select data from.
#' @param parameters The parameters to download. Breakdowns, dimensions and metrics.
#' @param api_version The version of Facebook API.
#' @export
#' @examples
#' fetch_fb_data(object=c("me","accounts","campaigns",fb_userId()), scope=c("adaccounts","insights"), parameters="", api_version="3.2")
#' @import httr
fetch_fb_data <- function(object, scope = "", parameters = "", api_version = "3.2"){
  if(!fb_check_existing_token()){
    stop("No authenticated token found!")
  }
  data <- httr::GET(url = paste0(
    paste0("https://graph.facebook.com/v",api_version,"/"),
    object,"/",
    scope,
    parameters
  ),
  config(token = FacebookAuth$public_fields$token))
  data <- rjson::fromJSON(rawToChar(data$content))
  return(data)
}
