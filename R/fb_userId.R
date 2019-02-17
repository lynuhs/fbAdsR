#' Get the User ID for the authorized Facebook user
#'
#' This function will import the User ID for the authenticated user.
#' @export
#' @examples
#' fb_userId()
fb_userId <- function(){
  data <- fetch_fb_data("me")
  data <- data$id

  return(data)
}
