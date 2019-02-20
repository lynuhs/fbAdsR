#' Collect data from Facebook API
#'
#' This function will import the data for the chosen parameters.
#' @param request_string A string that contains the API call to GET from Facebook.
#' @param api_version The version of Facebook API.
#' @export
#' @examples
#' fetch_fb_data(request_string, api_version="3.2")
#' @import httr plyr
fetch_fb_data <- function(request_string, api_version = "3.2"){
  if(!fb_check_existing_token()){
    stop("No authenticated token found!",call. = FALSE)
  }
  data <- httr::GET(url = paste0("https://graph.facebook.com/v",api_version,"/", request_string),
                    config(token = FacebookAuth$public_fields$token))

  if(data$status_code != 200){
    stop("Failed to make request to Facebook! Make sure to check your parameters, choose a shorter time range or use less granularity!")
  }
  data <- rjson::fromJSON(rawToChar(data$content))

  if(!(is.null(data$data))){
    data <- unlist(data$data)

    df <- NULL
    for(l in 1:(length(data))){
      temp_df <- data.frame(temp = matrix(1))
      for(i in 1:(length(data[[l]]))){
        column <- unlist(data[[l]][i])
        if(length(column) > 1){
          nest <- data.frame(matrix(column, ncol=2, byrow=TRUE))
          cName <- as.character(nest[,1])
          nest <- data.frame(matrix(nest[,2], nrow=1))
          colnames(nest) <- cName

          temp_df <- cbind(temp_df, nest)
        } else {
          cName <- names(column)
          column <- data.frame(column)
          colnames(column) <- cName

          temp_df <- cbind(temp_df,column)
        }
      }
      df <- plyr::rbind.fill(df, temp_df)
    }
    df <- df[-1]

    for(i in 1:(ncol(df))){
      if(grepl("date", colnames(df)[i])){
        df[,i] <- as.Date(df[,i])
      } else if (grepl("id$", colnames(df[i]))){
        df[,i] <- as.character(df[,i])
      } else{
        df[,i] <- type.convert(df[,i])
      }
    }

    if(all(c("date_start","date_stop") %in% colnames(df))){
      if(all(df$date_start == df$date_stop)){
        df <- df[,!(colnames(df) %in% "date_stop")]
        colnames(df) <- gsub("date_start","date",colnames(df))
      }
    }

    return(df)
  } else {
    return(data)
  }
}
