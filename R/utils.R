#' @import checkmate
#' @import cli
#' @importFrom data.table data.table merge.data.table set := setnames setkeyv
#' @importFrom glue glue
#' @import httr2
#' @importFrom purrr map map_chr map_lgl map_int
#' @importFrom rlang %||%
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
  x
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

get_page = function(
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

get_data = function(
    survey_id, endpoint, cache_dir = NULL, per_page = 200L, max_pages = 5L,
    query_args = NULL, body_arg = NULL, method = NULL) {

  # TODO: be smarter about max_pages and cache, but this is tricky
  if (is.null(max_pages)) max_pages = Inf

  if (!is.null(cache_dir)) {
    arg_hash = rlang::hash(list(query_args, body_arg)) # nolint
    cache_str = '{survey_id}_{endpoint}_{per_page}_{max_pages}_{arg_hash}.qs'
    cache_file = file.path(cache_dir, glue(cache_str))
    if (file.exists(cache_file)) {
      data = qs::qread(cache_file)
      cli_alert_success('Loaded data from cache')
      return(data)
    }
  }

  args_one = list(
    survey_id = survey_id, endpoint = endpoint, per_page = per_page,
    query_args = query_args, body_arg = body_arg, method = method)
  page_one = do.call(get_page, args_one)

  args_one$num_pages = page_one$last_page
  ok_pages = min(args_one$num_pages, max_pages)
  pages = list(page_one)

  if (ok_pages > 1L) {
    args_one$per_page = page_one$per_page # might not be as requested
    page_more = map(2:ok_pages, \(page) {
      do.call(get_page, c(args_one, page = page))
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

get_answers_per_entry = function(entries, answers_base, field_ids) {
  ape_list = map(entries, \(entry) {
    as.list(map_chr(entry$answers, 'text', .default = NA))
  })
  ape = rbindlist(ape_list)
  setnames(ape, paste0('field_id_', field_ids))
  ape = cbind(answers_base, ape)
}

get_answers_per_field = function(entries, answers_base, field_ids) {
  cols = setdiff(names(entries[[1L]]$answers[[1L]]), c('field', 'array'))
  apf_list = map(entries, \(entry) {
    rbindlist(map(entry$answers, \(x) x[cols]), idcol = 'answer_row')
  })
  apf = rbindlist(apf_list, idcol = 'entry_row')
  set(apf, j = 'field_id', value = field_ids[apf$answer_row])
  setnames(apf, c('id', 'text'), \(x) paste0('answer_', x))
  apf = merge.data.table(answers_base, apf, by = 'entry_row')
}

get_answers_per_option = function(entries, answers_base) {
  apo_list = map(entries, \(entry) {
    apo_now = map(entry$answers, \(answer) {
      x = unlist(answer$array)
      if (length(x) == 0L) return(NULL)
      data.table(
        answer_id = answer$id,
        field_id = answer$field$id,
        option_id = names(x) %||% paste0('dummy_', seq_len(length(x))),
        option_name = x)
    })
    if (all(lengths(apo_now) == 0L)) return(NULL)
    apo_now = rbindlist(
      apo_now, use.names = TRUE, fill = TRUE, idcol = 'answer_row')
    set(apo_now, j = 'option_other',
        value = apo_now$option_id == 'other_selected')
    suppressWarnings(
      set(apo_now, j = 'option_id', value = as.integer(apo_now$option_id)))
  })
  apo = rbindlist(apo_list, use.names = TRUE, fill = TRUE, idcol = 'entry_row')
  apo = merge.data.table(answers_base, apo, by = 'entry_row')
}
