#' Import statistics from Facebook
#'
#' This function will import the statistics for the chosen parameters.
#' @param account_id The account id for which you want to import data fram (needs to start with "act_").
#' @param fromDate The starting date for the time range you want.
#' @param toDate The ending date for the time range you want.
#' @param dimensions The dimensions to use in the API request.
#' @param metrics The metrics to use in the API request.
#' @param breakdowns What breakdowns should be used, e.g. age, gender etc.
#' @param level The level in which you want the data returned c("account","campaign","adset","ad")
#' @export
#' @examples
#' fb_stats(account_id, fromDate, toDate, dimensions, metrics, breakdowns = "", level="campaign")
fb_stats <- function(account_id, fromDate, toDate, dimensions, metrics, breakdowns = "", level="campaign"){
  if ("date" %in% dimensions){
    dimensions <- dimensions[which(dimensions!="date")]
    time <- "&time_increment=1"
  } else {
    time <- ""
  }

  breakdowns <- paste(breakdowns, collapse=",")
  dimensions <- paste(dimensions, collapse=",")
  metrics <- paste(metrics, collapse=",")

  fields <- paste(c(dimensions,metrics), collapse = ",")


  if(length(account_id) > 1){
    df <- NULL
    dimensions[!(dimensions %in% "account_id")]
    for(i in 1:length(account_id)){
      request <- paste0(
        account_id[i],
        "/insights?",
        "time_range={'since':'",as.character(fromDate),"','until':'",as.character(toDate),"'}",
        time,
        "&breakdowns=", breakdowns,
        "&fields=", fields,
        "&level=", level,
        "&limit=99999"
      )
      df <- rbind(df, cbind(account_id=gsub("act_","",account_id[i]), fetch_fb_data(request)))
    }
  } else {
    request <- paste0(
      account_id,
      "/insights?",
      "time_range={'since':'",as.character(fromDate),"','until':'",as.character(toDate),"'}",
      time,
      "&breakdowns=", breakdowns,
      "&fields=", fields,
      "&level=", level,
      "&limit=99999"
    )
    df <- fetch_fb_data(request)
  }


  return(df)
}
