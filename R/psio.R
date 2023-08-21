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
#'
#' @export
psio_get_api_key = function() {
  key = Sys.getenv('RPAPERSURVEY_KEY')
  if (!identical(key, '')) return(invisible(key))
  if (is_testing()) return(invisible(testing_key()))
  cli_abort('No API key found, please call `psio_set_api_key(...)`')
}

# ---- Survey API endpoints ----

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
  surveys = rbindlist(map(resp, \(x) x[!idx]), use.names = TRUE, fill = TRUE)
  for (col in names(which(idx))) {
    val = map(resp, col, .default = list())
    set(surveys, j = col, value = val)
  }
  set_to_posix(surveys)
}

# ---- Version API endpoints ----

#' Get survey versions, i.e., variants
#'
#' @param survey_id Integer indicating survey id.
#'
#' @export
psio_get_versions = function(survey_id) {
  assert_int(survey_id, lower = 1L)
  resp = req_start() %>%
    req_url_path_append(survey_id, 'versions') %>%
    req_finish()
  versions = rbindlist(resp, use.names = TRUE, fill = TRUE)
  setnames(versions, 'id', 'version_id')
  set_to_posix(versions)
}

# ---- Review API endpoints ----

#' Get Entry Responses for review
#' Similar to getting entries via the Data API but more compact, with extra
#' metadata used when verifying/reviewing answers.
#'
#' @param survey_id Integer indicating survey id.
#' @param cache_dir Path to directory in which to cache results.
#' @param per_page Integer indicating how to chunk responses.
#'
#' @export
psio_get_responses_for_review = function(survey_id,
                                         cache_dir = NULL,
                                         per_page = 200L) {
  assert_int(survey_id, lower = 1L)
  assert_string(cache_dir, null.ok = TRUE)
  assert_int(per_page, lower = 1L)
  psio_get_data(survey_id, 'review', cache_dir, per_page, use_post = TRUE)
}

#' Replace an answer to a particular question with your new response.
#'
#' @param survey_id Integer indicating survey id.
#' @param response_field_id Integer indicating the response field id as seen in
#' /surveys/:survey/review, or from the "answers > id" in entries.
#' @param answer String | list | Integer to replace the answer with; use a list
#' for single/multiple choice questions
#'
#' @export
psio_set_answer = function(survey_id, response_field_id, answer) {
  assert_int(survey_id, lower = 1L)
  assert_int(response_field_id, lower = 1L)
  req_start() %>%
    req_method('PUT') %>%
    req_url_path_append(survey_id, 'verify', response_field_id) %>%
    req_body_json(list(answer = answer)) %>%
    req_finish()
}

#' Mark selected answer(s) as *not verified*.
#'
#' @param survey_id Integer indicating survey id.
#' @param response_fields array of Integers indicating the response field ids as
#' seen in /surveys/:survey/review, or from the "answers > id" in entries.
#'
#' @export
psio_flag_answers_to_verify = function(survey_id, response_field_ids) {
  assert_int(survey_id, lower = 1L)
  assert_integer(response_field_ids, lower = 1L)
  req_start() %>%
    req_method('PUT') %>%
    req_url_path_append(survey_id, 'bulk', 'flag-to-verify') %>%
    req_body_json(list(ids = response_field_ids)) %>%
    req_finish(json_response = FALSE)
}

#' Mark selected answer(s) as *verified*.
#'
#' @param survey_id Integer indicating survey id.
#' @param response_fields array of Integers indicating the response field ids as
#' /surveys/:survey/review, or from the "answers > field > id" in entries.
#'
#' @export
psio_verify_answers = function(survey_id, response_field_ids) {
  assert_int(survey_id, lower = 1L)
  assert_integer(response_field_ids, lower = 1L)
  req_start() %>%
    req_method('PUT') %>%
    req_url_path_append(survey_id, 'bulk', 'verify') %>%
    req_body_json(list(ids = response_field_ids)) %>%
    req_finish(json_response = FALSE)
}

# ---- Data API endpoints & extraction helpers ----

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
  docs = rbindlist(data, use.names = TRUE, fill = TRUE)
  setnames(docs, 'id', 'document_id')
  set_to_posix(docs)
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

  fields = rbindlist(map(entries[[1L]]$answers, 'field'))
  setnames(fields, c('id', 'name'), \(x) paste0('field_', x))
  set(fields, j = 'field_id_str', value = paste0('field_id_', fields$field_id))
  if (!is.null(recoding)) {
    fields = data.table::merge.data.table(
      fields, data.table::as.data.table(recoding),
      by = 'field_id', all.x = TRUE, sort = FALSE)
    set(fields, i = which(is.na(fields$field_name_new)),
        j = 'field_name_new', value = fields$field_name)
  }
  fields[]
}

#' Get answers
#'
#' @param entries Result returned by [psio_get_entries()].
#' @param format String indicating whether to return simple or detailed results.
#'
#' @export
psio_get_answers = function(entries, format = c('simple', 'detailed')) {
  assert_list(entries)
  format = match.arg(format)

  field_ids = map_int(entries[[1L]]$answers, \(x) x$field$id)
  cols = setdiff(names(entries[[1L]]), c('answers', 'notes', 'pages'))
  answers_base = rbindlist(
    map(entries, \(entry) entry[cols]), idcol = 'entry_row')

  if (format == 'simple') {
    answers_list = map(entries, \(entry) {
      as.list(map_chr(entry$answers, 'text', .default = NA))
    })

    answers_simp = rbindlist(answers_list)
    setnames(answers_simp, paste0('field_id_', field_ids))
    answers = cbind(answers_base, answers_simp)

  } else {
    cols = setdiff(
      names(entries[[1L]]$answers[[1L]]), c('field', 'image', 'array'))
    answers_list = map(entries, \(entry) {
      rbindlist(map(entry$answers, \(x) x[cols]), idcol = 'answer_row')
    })

    answers_deet = rbindlist(answers_list, idcol = 'entry_row')
    set(answers_deet, j = 'field_id',
        value = field_ids[answers_deet$answer_row])
    setnames(answers_deet, c('id', 'text'), \(x) paste0('answer_', x))

    answers = data.table::merge.data.table(
      answers_base, answers_deet, by = 'entry_row')
  }

  set_to_posix(answers)
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
