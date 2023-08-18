#' @import checkmate
#' @import cli
#' @importFrom data.table rbindlist set := setnames
#' @importFrom glue glue
#' @import httr2
#' @importFrom purrr map map_chr map_lgl map_int
NULL

# TODO: add documentation
# TODO: add tests, ideally using a test account

is_testing = function() {
  identical(Sys.getenv('TESTTHAT'), 'true')
}

testing_key = function() {
  # random_key = secret_make_key()
  # usethis::edit_r_environ('project'): RPAPERSURVEY_KEY=<value_of_random_key>
  # added to github repo: secret RPAPERSURVEY_KEY with value of random_key
  # added to .gitignore: .Renviron
  # added to .Rbuildignore: ^\.Renviron$
  # secret_scrambled = secret_encrypt(readLines('PATH_TO_API_KEY'), random_key)
  secret_decrypt(
    paste0('secret_', 'scrambled'), # should correspond to a test account
    'RPAPERSURVEY_KEY')
}

req_start = function() {
  request('https://api.papersurvey.io/surveys')
}

req_finish = function(req) {
  req %>%
    req_url_query(api_token = psio_get_api_key()) %>%
    req_user_agent(
      'rpapersurvey (https://github.com/agency-fund/rpapersurvey)') %>%
    req_throttle(60 / 60) %>%
    req_perform() %>%
    resp_body_json()
}

psio_get_page = function(
    survey_id, endpoint, per_page = 200L, page = 1L, num_pages = NULL) {
  suf = if (is.null(num_pages)) '' else paste(' of', num_pages)
  cli_alert_info('Fetching page {page}{suf}')
  resp = req_start() %>%
    req_url_path_append(survey_id, endpoint) %>%
    req_url_query(per_page = per_page, perPage = per_page, page = page) %>%
    req_finish()
  cli_alert_success('Fetched page {page}{suf}')
  resp
}

psio_get_data = function(
    survey_id, endpoint, cache_dir = NULL, per_page = 200L) {

  if (!is.null(cache_dir)) {
    cache_file = file.path(
      cache_dir, glue('survey_id_{survey_id}_{endpoint}.qs'))
    if (file.exists(cache_file)) {
      data = qs::qread(cache_file)
      cli_alert_success('Loaded data from cache')
      return(data)
    }
  }

  page_one = psio_get_page(survey_id, endpoint, per_page)
  num_pages = page_one$last_page
  per_page = page_one$per_page # might not be as requested
  pages = list(page_one)
  if (num_pages > 1L) {
    page_more = map(2:num_pages, \(page) {
      psio_get_page(survey_id, endpoint, per_page, page, num_pages)
    })
    pages = c(pages, page_more)
  }
  data = unlist(map(pages, 'data'), recursive = FALSE)

  if (!is.null(cache_dir)) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir)
    qs::qsave(data, cache_file)
    cli_alert_success('Saved data to cache')
  }
  data
}
