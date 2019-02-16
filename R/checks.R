#' Check that token appears to be legitimate
#'
#' Catch tokens that are technically valid,
#' i.e. `inherits(token, "Token2.0")` is TRUE, but that have dysfunctional
#' credentials.
#'
#' @keywords internal
#' @family authentication functions
#' @noRd
is_legit_token <- function(x) {

  if(!inherits(x, "Token2.0")) {
    cat(crayon::red(paste0("Not a Token2.0 object. Found:", class(x), "\n")))
    if(!inherits(x, "list")){
      if(inherits(x[[1]], "Token2.0")){
        warning("Passed a list of Token2.0 objects, not a Token2.0 object.")
        return(FALSE)
      }
    }
    return(FALSE)
  }

  if("invalid_client" %in% unlist(x$credentials)) {
    # check for validity so error is found before making requests
    # shouldn't happen if id and secret don't change
    cat(crayon::red("Invalid client authorization error. Check client_id and client_secret.\n"))

    return(FALSE)
  }

  if("invalid_request" %in% unlist(x$credentials)) {
    # known example: if user clicks "Cancel" instead of "Accept" when OAuth2
    # flow kicks to browser
    cat(crayon::red("Invalid request authorization error. Check request format.\n"))
    return(FALSE)
  }

  TRUE
}




#' Check a token vs options
#'
#' Useful for debugging authentication issues
#'
#' @param token A token to check, default current live session token
#'
#' @return \code{FALSE} if the options and current token do not match, \code{TRUE} if they do.
#'
#' @details
#'
#' Will compare the passed token's settings and compare to set options.
#'   If these differ, then reauthentication may be needed.
#'
#' @export
fb_check_existing_token <- function(token = FacebookAuth$public_fields$token){

  cache_path <- client_id <- client_secret <- FALSE

  if(is.null(token)){
    cat(crayon::red("No local token found in session\n"))
    return(FALSE)
  }

  cache_path <- is.different(token$cache_path, "fbAdsR.httr_oauth_cache")

  if(!is.null(token$app)){
    client_id     <- is.different(token$app$key, "fbAdsR.client_id")
    client_secret <- is.different(token$app$secret, "fbAdsR.client_secret")

  } else {
    cat(crayon::red("No client_id in token, authentication from JSON key file\n"))
  }

  ## FALSE if any are different, TRUE if they are not
  !any(cache_path, client_id, client_secret)
}

is.different <- function(token_element, option_name){
  if(!all(token_element %in% getOption(option_name))){
    cat(crayon::red(sprintf("Token %s != getOption('%s') \n#>Token: %s \n#>Option: %s\n",
                            deparse(substitute(token_element)),
                            option_name,
                            paste(token_element, collapse = " "),
                            paste(getOption(option_name), collapse = " "),
                            "\n"
    )))

    return(TRUE)
  }
  FALSE
}



is.different <- function(token_element, option_name){
  if(!all(token_element %in% getOption(option_name))){
    cat(crayon::red(sprintf("Token %s != getOption('%s') \n#>Token: %s \n#>Option: %s\n",
                            deparse(substitute(token_element)),
                            option_name,
                            paste(token_element, collapse = " "),
                            paste(getOption(option_name), collapse = " "),
                            "\n"
    )))

    return(TRUE)
  }
  FALSE
}
