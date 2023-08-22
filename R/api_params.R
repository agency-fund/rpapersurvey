#' Set the API key
#'
#' @param path Path to a text file containing an API key.
#'
#' @export
psio_set_api_key = function(path = NULL) {
  key = if (is.null(path)) {
    askpass::askpass('Please enter your API key')
  } else {
    assert_string(path)
    assert_file_exists(path)
    readLines(path, n = 1L, warn = FALSE)
  }
  Sys.setenv(RPAPERSURVEY_KEY = key)
  invisible()
}

#' Get the API key
#'
#' @export
psio_get_api_key = function() {
  key = Sys.getenv('RPAPERSURVEY_KEY')
  if (!identical(key, '')) return(invisible(key))
  if (is_testing()) return(invisible(testing_key()))
  cli_abort('No API key found, please call `psio_set_api_key(...)`')
}

#' Get data for the current user
#'
#' @export
psio_get_user = function() {
  req_start('https://www.papersurvey.io/api/user') %>%
    req_finish()
}

#' Set the team id for API calls
#'
#' @param team_id Integer indicating the team id, as seen in [psio_get_user()].
#'
#' @export
psio_set_team = function(team_id = NULL) {
  assert_int(team_id, lower = 1L, null.ok = TRUE)
  team = if (is.null(team_id)) '' else team_id
  Sys.setenv(RPAPERSURVEY_TEAM = team)
}
