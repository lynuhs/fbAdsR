#' Collect data from Facebook API
#'
#' This function will import the data for the chosen parameters.
#' @param request_string A string that contains the API call to GET from Facebook.
#' @param api_version The version of Facebook API.
#' @export
#' @examples
#' fetch_fb_data(request_string, api_version="3.2")
#' @import httr plyr
fetch_fb_data <- function(request_string, api_version = "3.2", print.status = FALSE){
  if(!fb_check_existing_token()){
    tryCatch({
      fb_auth()
    }, error = function(e){
      stop("No authenticated token found!",call. = FALSE)
    })
  }

  if(regexpr("'since'", request_string) > 0 & regexpr("'until'", request_string) > 0){
    from <- substr(request_string, regexpr("'since'", request_string)+9, regexpr("'since'", request_string)+18)
    to <- substr(request_string, regexpr("'until'", request_string)+9, regexpr("'until'", request_string)+18)
    days <- as.numeric(difftime(to, from, units = "days"))+1
    splitted_days <- 20

    data <- NULL
    if(days > splitted_days){

      cat(crayon::red(paste0(
        "The requested amount of dates are too many for one API call. The request will be slit into ",
        ceiling(days/splitted_days),
        " requests instead.\n")))

      for(d in 1:(ceiling(days/splitted_days))){
        cat(crayon::red(paste0("Request ", d, " of ", ceiling(days/splitted_days), " is running...")))
        splitFrom <- as.Date(from) + ((d-1)*splitted_days)
        splitTo <- as.Date(from) + ((d-1)*splitted_days)+(splitted_days-1)
        if(splitTo > as.Date(to)){
          splitTo <- as.Date(to)
        }

        split_request <- gsub(to, splitTo, gsub(from, splitFrom, request_string))

        data <- plyr::rbind.fill(data, fetch_data(split_request, print.status = FALSE))
        cat(crayon::red(paste0("completed!\n")))
      }

    } else {
      data <- fetch_data(request_string, print.status = print.status)
    }
  } else {
    data <- fetch_data(request_string, print.status = print.status)
  }

  return(data)
}


#' Structures relevance score in a correct way
#'
#' Takes a data frame of relevance score columns and changes them to one column with integer values
#'
#'
#' @noRd
relevance_score <- function(data){
  data <- data[!(grepl("status",colnames(data)))]
  for(i in 1:ncol(data)){
    score <- gsub("\\.","",substr(colnames(data[i]), nchar(colnames(data[i]))-1, nchar(colnames(data[i]))))
    data[,i] <- as.integer(gsub("OK", score, data[,i]))
  }
  data[is.na(data)] <- 0
  data$relevance_score <- rowSums(data)
  data[data==0] <- NA
  data <- as.data.frame(data[,'relevance_score'])
  colnames(data) <- "ad_relevance_score"
  return(data)
}


#' Sub routine to collect facebook data.
#'
#' @import httr plyr
#' @noRd
fetch_data <- function(request_string, api_version = "3.2", print.status = FALSE){
  if(print.status){
    cat(crayon::red("Collecting data from Facebook...\n"))
  }
  data <- httr::GET(url = paste0("https://graph.facebook.com/v",api_version,"/", request_string),
                    config(token = FacebookAuth$public_fields$token))

  if (data$status_code == 500){
    stop("[500] Internal server error. Try a shorter time period or less granularity!")
  } else if(data$status_code != 200){
    stop("Failed to make request to Facebook! Make sure to check your parameters, choose a shorter time range or use less granularity!")
  }
  data <- rjson::fromJSON(rawToChar(data$content))
  if(print.status){
    cat(crayon::red("Walking through data...\n"))
  }
  if(!(is.null(data$data))){
    data <- data$data
    df <- NULL
    for(l in 1:(length(data))){
      temp_df <- data.frame(temp = matrix(1))
      for(i in 1:(length(data[[l]]))){
        column <- unlist(data[[l]][i])
        if(length(column) > 1){
          if(any(grepl("pixel",unlist(data[[l]][i])))){
            main_name <- unlist(strsplit(names(column)[1], "\\."))[1]
            loops <- length(data[[l]][i][[1]])
            nested_df <- data.frame(temp = matrix(1))
            nested_names <- NULL
            for(m in 1:loops){
              nested_columns_length <- length(unlist(data[[l]][i][[1]][m]))
              nested_df <- cbind(nested_df, data.frame(unlist(data[[l]][i][[1]][m])['value'], row.names = 1))
              nested_names <- c(nested_names, paste0(main_name,".",unlist(data[[l]][i][[1]][m])[1]))
              if(nested_columns_length > 2){
                for(k in 3:nested_columns_length){
                  temp_nest_df <- data.frame(unlist(data[[l]][i][[1]][m])[k], row.names = 1)
                  nested_names <- c(nested_names, paste0(main_name,".",unlist(data[[l]][i][[1]][m])[1], ".", names(unlist(data[[l]][i][[1]][m])[k])))
                  nested_df <- cbind(nested_df, temp_nest_df)
                }
              }
            }
            nested_df <- nested_df[-1]
            colnames(nested_df) <- nested_names
            temp_df <- cbind(temp_df, nested_df)
          } else {
            nest <- data.frame(matrix(column, ncol=2, byrow=TRUE))
            nestName <- unlist(strsplit(names(column)[1], "\\."))[1]
            cName <- paste0(nestName,".",as.character(nest[,1]))
            nest <- data.frame(matrix(nest[,2], nrow=1))
            colnames(nest) <- cName

            temp_df <- cbind(temp_df, nest)
          }
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

    if(any(grepl("relevance_score", colnames(df)))){
      rel <- relevance_score(df[grepl("relevance_score", colnames(df))])
      df <- cbind(df[!(grepl("relevance_score", colnames(df)))], rel)
    }

    if(print.status){
      cat(crayon::green("Operation finished successfully!\n"))
    }

    return(df)
  } else {
    if(print.status){
      cat(crayon::green("Operation finished successfully!\n"))
    }
    return(data)
  }
}
