#' Get documents
#'
#' @param survey_id Integer indicating survey id.
#' @param per_page Integer indicating how to chunk responses.
#'
#' @export
psio_get_documents = function(survey_id, per_page = 200L) {
  assert_int(survey_id, lower = 1L)
  assert_int(per_page, lower = 1L)
  data = psio_get_data(
    survey_id, 'documents', per_page = per_page, max_pages = NULL)
  docs = rbindlist(data, use.names = TRUE, fill = TRUE)
  setnames(docs, 'id', 'document_id')
  set_to_posix(docs)
}

#' Get entries
#'
#' @param survey_id Integer indicating survey id.
#' @param cache_dir Path to directory in which to cache results.
#' @param per_page Integer indicating how to chunk responses.
#' @param max_pages Integer indicating maximum number of pages to fetch.
#'
#' @export
psio_get_entries = function(
    survey_id, cache_dir = NULL, per_page = 200L, max_pages = 5L) {
  assert_int(survey_id, lower = 1L)
  assert_string(cache_dir, null.ok = TRUE)
  assert_int(per_page, lower = 1L)
  assert_int(max_pages, lower = 1L, null.ok = TRUE)
  psio_get_data(survey_id, 'entries', cache_dir, per_page, max_pages)
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
  resp = req_start() %>%
    req_url_path_append(survey_id, 'entries', 'questions') %>%
    req_finish()
  questions = rbindlist(
    resp, use.names = TRUE, fill = TRUE, check_list_cols = TRUE)
  setnames(questions, c('id', 'name'), \(x) paste0('field_', x))[]
}
