#' Import statistics from Facebook
#'
#' This function will import the statistics for the chosen parameters.
#' @param account_id The account id for which you want to import data fram (needs to start with "act_").
#' @param fromDate The starting date for the time range you want.
#' @param toDate The ending date for the time range you want.
#' @param dimensions The dimensions to use in the API request.
#' @param metrics The metrics to use in the API request.
#' @param level The level in which you want the data returned (defaults to "campaign")
#' @export
#' @examples
#' fb_stats(account_id, fromDate, toDate, dimensions, metrics, level=c("account","campaign","adset","ad"))
fb_stats <- function(account_id, fromDate, toDate, dimensions, metrics, level="campaign"){
  if ("date" %in% dimensions){
    dimensions <- dimensions[which(dimensions!="date")]
    time <- "&time_increment=1"
  } else {
    time <- ""
  }

  dimensions <- paste(dimensions, collapse=",")
  metrics <- paste(metrics, collapse=",")

  fields <- paste(c(dimensions,metrics), collapse = ",")
  request <- paste0(
    account_id,
    "/insights?",
    "time_range={'since':'",as.character(fromDate),"','until':'",as.character(toDate),"'}",
    time,
    "&fields=", fields,
    "&level=", level,
    "&limit=99999"
  )


  df <- fetch(request_string = request)

  return(df)
}