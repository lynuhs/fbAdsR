#' List all ad accounts associated with the authorized user
#'
#' @export
#' @examples
#' fb_adAccounts()
fb_adAccounts <- function(){
  data <- fetch_fb_data(object = fb_userId(), scope = "adaccounts")
  data <- unlist(data)
  accounts <- as.character(data[names(data) == "data.id"])

  return(accounts)
}
