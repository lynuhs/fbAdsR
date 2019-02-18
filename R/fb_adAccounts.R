#' List all ad accounts associated with the authorized user
#'
#' @export
#' @examples
#' fb_adAccounts()
fb_adAccounts <- function(){
  request <- paste0(fb_userId(),"/adaccounts")
  data <- fetch_fb_data(request)

  return(data$id)
}
