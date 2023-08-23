#' @import checkmate
#' @import cli
#' @importFrom data.table set := setnames
#' @importFrom glue glue
#' @import httr2
#' @importFrom purrr map map_chr map_lgl map_int
NULL

utils::globalVariables('.')

# TODO: revise documentation, esp. description, return values, and examples
# TODO: add tests

is_testing = function() {
  identical(Sys.getenv('TESTTHAT'), 'true')
}

testing_key = function() {
  # random_key = secret_make_key()
  # usethis::edit_r_environ('project'): RPAPERSURVEY_KEY=<value_of_random_key>
  # add to github repo: secret RPAPERSURVEY_KEY with value of random_key
  # add to .gitignore: .Renviron
  # add to .Rbuildignore: ^\.Renviron$
  # Restart R to set the env var to value of random_key
  # encrypted = secret_encrypt(readLines('PATH_TO_API_KEY'), 'RPAPERSURVEY_KEY')
  secret_decrypt(
    paste0('U5R9B0zqKoVsFB1v-61fjnMqeKQ5Lq2hFVkRmbe5QPvsl3RYhr4', # encrypted
           'wJYRsUOIVqxDkfY0a39qsxX61IMz7lNJmgGAHTeFeJryZOVpVvw'),
    'RPAPERSURVEY_KEY')
}

rbindlist = function(l, ..., check_list_cols = FALSE) {
  suppressWarnings({
    if (isTRUE(check_list_cols)) {
      cols = unique(unlist(map(l, names)))
      idx = map_lgl(cols, \(col) any(map_lgl(l, \(x) is.list(x[[col]]))))
      r = data.table::rbindlist(
        map(l, \(x) x[names(x) %in% cols[!idx]]), ...)
      for (col in cols[idx]) {
        val = map(l, col, .default = list())
        set(r, j = col, value = val)
      }
      r
    } else {
      data.table::rbindlist(l, ...)
    }
  })
}

set_to_posix = function(
    x, cols = c('created_at', 'uploaded_at', 'updated_at'),
    format = '%Y-%m-%dT%H:%M:%S') {
  cols_now = intersect(cols, colnames(x)[map_lgl(x, is.character)])
  for (col in cols_now) {
    set(x, j = col, value = as.POSIXct(x[[col]], tz = 'UTC', format = format))
  }
  x[]
}

req_start = function(
    base_url = 'https://api.papersurvey.io/surveys', method = NULL) {
  request(base_url) %>%
    req_headers(
      Authorization = paste('Bearer', psio_get_api_key()),
      `X-Team` = Sys.getenv('RPAPERSURVEY_TEAM')) %>%
    req_user_agent(
      'rpapersurvey (https://github.com/agency-fund/rpapersurvey)') %>%
    req_throttle(60 / 60) %>%
    {if (is.null(method)) . else req_method(., method)} # nolint
}

req_finish = function(req, dry_run = FALSE, json_response = TRUE) {
  if (isTRUE(dry_run)) {
    req %>%
      req_dry_run()
  } else {
    req %>%
      req_perform() %>%
      {if (isTRUE(json_response)) resp_body_json(.) else .} # nolint
  }
}

psio_get_page = function(
    survey_id, endpoint, per_page = 200L, page = 1L, num_pages = NULL,
    query_args = NULL, body_arg = NULL, method = NULL) {

  suf = if (is.null(num_pages)) '' else paste(' of', num_pages)
  cli_alert_info(paste0('Fetching page ', page, suf))

  req = req_start(method = method) %>%
    req_url_path_append(survey_id, endpoint) %>%
    req_url_query(per_page = per_page, perPage = per_page, page = page)

  req = do.call(req_url_query, c(list(.req = req), query_args)) # pipe no work
  if (!is.null(body_arg)) req = req_body_json(req, body_arg)
  resp = req_finish(req)

  cli_alert_success('Fetched page {page}{suf}')
  resp
}

psio_get_data = function(
    survey_id, endpoint, cache_dir = NULL, per_page = 200L, max_pages = 5L,
    query_args = NULL, body_arg = NULL, method = NULL) {

  # TODO: be smarter about max_pages and cache, but this is tricky
  if (is.null(max_pages)) max_pages = Inf

  if (!is.null(cache_dir)) {
    arg_hash = rlang::hash(list(query_args)) # nolint
    cache_str = '{survey_id}_{endpoint}_{per_page}_{max_pages}_{arg_hash}.qs'
    cache_file = file.path(cache_dir, glue(cache_str))
    if (file.exists(cache_file)) {
      data = qs::qread(cache_file)
      cli_alert_success('Loaded data from cache')
      return(data)
    }
  }

  # page_one = psio_get_page(
  #   survey_id, endpoint, per_page, query_args = query_args,
  #   body_arg = body_arg, method = method)
  args_one = list(
    survey_id = survey_id, endpoint = endpoint, per_page = per_page,
    query_args = query_args, body_arg = body_arg, method = method)
  page_one = do.call(psio_get_page, args_one)

  num_pages = page_one$last_page
  ok_pages = min(num_pages, max_pages)
  per_page = page_one$per_page # might not be as requested
  pages = list(page_one)

  if (ok_pages > 1L) {
    page_more = map(2:ok_pages, \(page) {
      args_more = list(page = page, num_pages = num_pages)
      do.call(psio_get_page, c(args_one, args_more))
      # psio_get_page(
      #   survey_id, endpoint, per_page, page = page, num_pages = num_pages,
      #   query_args = query_args, body_arg = body_arg, method = method)
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
