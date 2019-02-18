#' List all campaigns for the chosen ad accounts
#'
#' @param account_id The id of one or more ad account ids (act_XXXXXXX)
#' @export
#' @examples
#' fb_campaigns(account_id = fb_adAccounts())
fb_campaigns <- function(account_id = fb_adAccounts()){
  account_id <- ifelse(grepl("act_", account_id), account_id, paste0("act_",account_id))
  ids <- NULL

  for(i in 1:(length(account_id))){
    data <- fetch_fb_data(paste0(account_id[i], "/campaigns"))
    ids <- rbind(ids, data.frame(account_id  = account_id[i],
                                 campaign_id = data$id))
  }
  return(ids)
}
