#' R6 environment to store authentication credentials
#'
#' Used to keep persistent state.
#' @export
FacebookAuth <- R6::R6Class(
  "FacebookAuth",
  public = list(
    token = NULL,
    method = NULL
  ),
  lock_objects = FALSE,
  parent_env = emptyenv()
)



#' @export
#' @family authentication functions
#' @import assertthat
fb_auth <- function(key = Sys.getenv("FB_CLIENT_ID"), secret = Sys.getenv("FB_CLIENT_SECRET"), token = NULL, new_user = FALSE){
  if(key == "" | secret == ""){
    stop("Need a valid Client ID and Client Secret in order to authorize connection!", call. = FALSE)
  } else {
    Sys.setenv(FB_CLIENT_ID = key)
    Sys.setenv(FB_CLIENT_SECRET = secret)
  }

  checkEnvFile <- function(env){
    value <- Sys.getenv(env)
    value <- ifelse(value == "", return(NULL), return(value))
  }

  options("fbAdsR.httr_oauth_cache" = ifelse(is.null(getOption("fbAdsR.httr_oauth_cache")),
                                             ifelse(is.null(token), ".httr-oauth", token),
                                             getOption("fbAdsR.httr_oauth_cache")))

  options("fbAdsR.client_id" = checkEnvFile("FB_CLIENT_ID"))
  options("fbAdsR.client_secret" = checkEnvFile("FB_CLIENT_SECRET"))


  httr_file <- getOption("fbAdsR.httr_oauth_cache")

  if(assertthat::is.flag(httr_file)){
    stop("option('fbAdsR.httr_oauth_cache') must be set to
         valid cache file location,
         not TRUE or FALSE - (example: '.httr-oauth')",
         call. = FALSE)
  }

  assertthat::assert_that(assertthat::is.string(httr_file),
                          assertthat::is.flag(new_user))


  if(new_user){
    rm_old_user_cache(httr_file)
  }


  if(is.null(token)) {     ## supplied no token

    facebook_token <- create_fb_token()

  } else if(is.token2.0(token)){     ## supplied a Token object

    legit <- is_legit_token(token)
    if(!legit){
      stop("Invalid token passed to function", call. = FALSE)
    }

    FacebookAuth$set("public", "method", "passed_token", overwrite=TRUE)
    ## set the global session token
    FacebookAuth$set("public", "token", token, overwrite=TRUE)

    ## just return it back
    facebook_token <- token

  } else if(assertthat::is.string(token)){ ## a filepath

    if(file.exists(token)){
      facebook_token <- read_cache_token(token_path = token)
    } else {
      cat(crayon::red(paste0("No httr_oauth_cache file found at ", token, " - creating new file.\n")))
      options("fbAdsR.httr_oauth_cache" = token)
      FacebookAuth$set("public", "token", NULL, overwrite=TRUE)
      return(fb_auth(token = NULL))
    }


  } else {
    stop("Unrecognised token object - class ", class(token), call. = FALSE)
  }

  fb_check_existing_token()

  ## return facebook_token above
  cat(crayon::green("Successfully authenticated Facebook API!\n"))
  return(invisible(facebook_token))

}



#' @noRd
#' @importFrom httr oauth_endpoints oauth_app oauth2.0_token
create_fb_token <- function(){
  check_existing <- fb_check_existing_token()
  if(!check_existing){
    cat(crayon::red("Auto-refresh of token not possible, manual re-authentication required\n"))

    if(!interactive()){
      stop("Authentication options didn't match existing session token and not interactive session
           so unable to manually reauthenticate", call. = FALSE)
    }
  }

  endpoint <- oauth_endpoints("facebook")

  key    <- getOption("fbAdsR.client_id", "")
  secret <- getOption("fbAdsR.client_secret", "")
  cache  <- getOption("fbAdsR.httr_oauth_cache", "")

  if(key == ""){
    stop("option('fbAdsR.client_id') has not been set", call. = FALSE)
  }

  if(secret == ""){
    stop("option('fbAdsR.client_secret') has not been set", call. = FALSE)
  }

  if(cache == ""){
    stop("option('fbAdsR.httr_oauth_cache') has not been set", call. = FALSE)
  }



  app <- oauth_app("facebook", key = key, secret = secret)


  facebook_token <- oauth2.0_token(endpoint = endpoint,
                                   app = app,
                                   cache = cache)


  stopifnot(is_legit_token(facebook_token))

  FacebookAuth$set("public", "token", facebook_token, overwrite=TRUE)
  FacebookAuth$set("public", "method", "new_token", overwrite=TRUE)

  facebook_token
}




#' @noRd
rm_empty_token <- function(token_path = getOption("fbAdsR.httr_oauth_cache")){
  ## delete token if 0B
  iz_0B <- file.info(token_path)$size == 0
  if(iz_0B){
    unlink(token_path)
  }
}




#' @noRd
rm_old_user_cache <- function(httr_file){
  FacebookAuth$set("public", "token", NULL, overwrite=TRUE)
  if(file.exists(httr_file)){
    cat(crayon::red(paste0("Removing old cached credentials from: ", normalizePath(httr_file),"\n")))
    file.remove(httr_file)
  }
}



#' Reads a token from a filepath
#'
#' Also sets the option of token cache name to the supplied filepath
#'   "fbAdsR.httr_oauth_cache"
#'
#' httr cache files such as .httr-oauth can hold multiple tokens for different scopes,
#'   this only returns the first one and raises a warning if there are multiple
#'   in the rds file
#' @noRd
#' @import assertthat
read_cache_token <- function(token_path){

  assertthat::assert_that(assertthat::is.readable(token_path))

  cat(crayon::red("Reading token from file path\n"))

  facebook_token <- tryCatch({readRDS(token_path)},
                             error = function(ex){
                               stop(sprintf("Cannot read token from alleged .rds file:\n%s",
                                            token_path),
                                    ex,
                                    call. = FALSE)
                             })

  if(is.list(facebook_token)){
    cat(crayon::red("Multiple httr-tokens in cache ",token_path, ", only returning first found token\n"))
    facebook_token <- facebook_token[[1]]
  } else if(is.token2.0(facebook_token)){
    cat(crayon::red("Read token successfully from file\n"))
  } else {
    stop("Unknown object read from ", token_path, " of class ", class(facebook_token))
  }

  ## for existing tokens, set the options to what is in the token
  facebook_token <- overwrite_options(facebook_token, token_path = token_path)

  FacebookAuth$set("public", "method", "filepath", overwrite=TRUE)
  ## set the global session token
  FacebookAuth$set("public", "token", facebook_token, overwrite=TRUE)

  facebook_token
}


fb_token_info <- function(detail_level = getOption("fbAdsR.verbose", default = 3)){
  token  <- FacebookAuth$public_fields$token
  method <- FacebookAuth$public_fields$method
  message <- ""

  if(is.null(token)){
    message <- c(message, "No token found\n")
    return(NULL)
  }
  if(detail_level >= 3){
    message <- c(message, paste0("Authentication from cache file: ", token$cache_path,"\n"))

    ## service
    if(!is.null(token$secrets)){
      message <- c(message, paste0("Type: ", token$secrets$type,"\n"))
      message <- c(message, paste0("ProjectID: ", token$secrets$project_id,"\n"))
      message <- c(message, paste0("Client email: ", token$secrets$client_email,"\n"))
      message <- c(message, paste0("ClientID: ", token$secrets$client_id,"\n"))
    }

  }

  if(detail_level <= 2){
    if(!is.null(token$app$key)){
      message <- c(message, paste0("App key: ", token$app$key,"\n"))
    }

    message <- c(message, paste0("Method: ", method,"\n"))

  }

  if(detail_level == 1){
    message <- c(message, paste0("Hash: ", token$hash(),"\n"))
  }

  cat(crayon::red(message))
}

overwrite_options <- function(facebook_token, token_path){
  options("fbAdsR.httr_oauth_cache" = token_path)
  facebook_token$cache_path <- token_path

  if(is.null(facebook_token$app)){
    cat(crayon::red("No client_id in token, authentication from JSON key file\n"))
    return(facebook_token)
  }

  if(is.different(facebook_token$app$key, "fbAdsR.client_id")){
    cat(crayon::red(paste0("Overwriting fbAdsR.client_id from", getOption("fbAdsR.client_id"),
                           "to ", facebook_token$app$key,"\n")))
    options("fbAdsR.client_id" = facebook_token$app$key)
  }

  if(is.different(facebook_token$app$secret, "fbAdsR.client_secret")){
    cat(crayon::red(paste0("Overwriting fbAdsR.client_secret to ", facebook_token$app$secret,"\n")))
    options("fbAdsR.client_secret" = facebook_token$app$secret)
  }

  facebook_token

}

is.token2.0 <- function(x){
  inherits(x, "Token2.0")
}




#' Retrieve Facebook token from environment and configs for httr
#'
#' Get token if it's previously stored, else prompt user to get one.
#' @param shiny_return_token In a shiny session, this is passed instead.
#' @return a httr configured option for token
#' For shiny the token is passed from reactive session
#'
#' @keywords internal
#' @family authentication functions
#' @importFrom httr config
get_facebook_token <- function(shiny_return_token=NULL) {

  if(any(which(grepl("with_mock_API", as.character(sys.calls()))))){
    cat(crayon::red("Skipping token checks as using with_mock_API\n"))
    return(NULL)
  }

  if(is.null(shiny_return_token)){
    token <- FacebookAuth$public_fields$token

    if(is.null(token) || !is_legit_token(token)) {
      fb_auth()
    }


  } else { #shiny session
    FacebookAuth$set("public", "method", "shiny", overwrite=TRUE)
    token <- shiny_return_token

  }

  config(token = token)

}
