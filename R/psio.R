#' Set the API key
#'
#' @param path Path to a text file containing an API key.
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
#' @export
psio_get_api_key = function() {
  key = Sys.getenv('RPAPERSURVEY_KEY')
  if (!identical(key, '')) return(invisible(key))
  if (is_testing()) return(invisible(testing_key()))
  stop('No API key found, please call `psio_set_api_key(...)`')
}

#' Get survey metadata
#'
#' @param survey_id Integer indicating survey id.
#'
#' @export
psio_get_surveys = function(survey_id = NULL) {
  assert_int(survey_id, lower = 1L, null.ok = TRUE)

  resp = req_start() %>%
    req_url_path_append(survey_id) %>%
    req_finish()
  if (!is.null(survey_id)) resp = list(resp)

  idx = map_lgl(resp[[1L]], is.list)
  surveys = suppressWarnings(
    rbindlist(map(resp, \(x) x[!idx]), use.names = TRUE, fill = TRUE))
  for (col in names(which(idx))) {
    val = map(resp, col, .default = list())
    set(surveys, j = col, value = val)
  }
  surveys
}

#' Get survey versions, i.e., variants
#'
#' @param survey_id Integer indicating survey id.
#'
#' @export
psio_get_versions = function(survey_id) {
  assert_int(survey_id, lower = 1L)
  req_start() %>%
    req_url_path_append(survey_id, 'versions') %>%
    req_finish()
}

#' Get documents
#'
#' @param survey_id Integer indicating survey id.
#' @param per_page Integer indicating how to chunk responses.
#'
#' @export
psio_get_documents = function(survey_id, per_page = 200L) {
  assert_int(survey_id, lower = 1L)
  assert_int(per_page, lower = 1L)
  data = psio_get_data(survey_id, 'documents', per_page = per_page)
  docs = suppressWarnings(rbindlist(data, use.names = TRUE, fill = TRUE))
  docs
}

#' Get entries
#'
#' @param survey_id Integer indicating survey id.
#' @param cache_dir Path to directory in which to cache results.
#' @param per_page Integer indicating how to chunk responses.
#'
#' @export
psio_get_entries = function(survey_id, cache_dir = NULL, per_page = 200L) {
  assert_int(survey_id, lower = 1L)
  assert_string(cache_dir, null.ok = TRUE)
  assert_int(per_page, lower = 1L)
  psio_get_data(survey_id, 'entries', cache_dir, per_page)
}

#' Get fields
#'
#' @param entries Result returned by [psio_get_entries()].
#' @param recoding `data.frame` with which to recode field names.
#'
#' @export
psio_get_fields = function(entries, recoding = NULL) {
  assert_list(entries)
  assert_data_frame(recoding, null.ok = TRUE)

  field_id_chr = field_id = field_name = field_name_new = NULL
  fields = rbindlist(map(entries[[1L]]$answers, 'field'))
  setnames(fields, c('id', 'name'), \(x) paste0('field_', x))
  fields[, field_id_chr := paste0('field_id_', field_id)]
  if (!is.null(recoding)) {
    fields = data.table::merge.data.table(
      fields, data.table::as.data.table(recoding),
      by = 'field_id', all.x = TRUE, sort = FALSE)
    fields[is.na(field_name_new), field_name_new := field_name]
  }
  fields[]
}

#' Get answers
#'
#' @param entries Result returned by [psio_get_entries()].
#'
#' @export
psio_get_answers = function(entries) {
  # skipping notes (NULL) and pages
  assert_list(entries)
  answers_list = map(entries, \(entry) {
    as.list(map_chr(entry$answers, 'text', .default = NA))
  })
  answers_text = rbindlist(answers_list)
  field_ids = paste0(
    'field_id_', map_int(entries[[1L]]$answers, \(x) x$field$id))
  setnames(answers_text, field_ids)

  cols = setdiff(names(entries[[1L]]), c('answers', 'notes', 'pages'))
  answers_other = rbindlist(map(entries, \(entry) entry[cols]))
  answers = cbind(answers_other, answers_text)
  answers
}

#' Get questions
#'
#' @param survey_id Integer indicating survey id.
#'
#' @export
psio_get_questions = function(survey_id) {
  assert_int(survey_id, lower = 1L)
  req_start() %>%
    req_url_path_append(survey_id, 'entries', 'questions') %>%
    req_finish()
}
