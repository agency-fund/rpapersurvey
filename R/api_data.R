#' Get documents
#'
#' @param survey_id Integer for survey id.
#' @param per_page Integer for how to chunk fetching of results.
#'
#' @export
psio_get_documents = function(survey_id, per_page = 200L) {
  assert_int(survey_id, lower = 1L)
  assert_int(per_page, lower = 1L, upper = 1000L)
  data = psio_get_data(
    survey_id, 'documents', per_page = per_page, max_pages = NULL)
  docs = rbindlist(data, use.names = TRUE, fill = TRUE)
  set_to_posix(docs)
  setnames(docs, 'id', 'document_id')
  setkeyv(docs, 'document_id')
}

#' Get entries
#'
#' @param survey_id Integer for survey id.
#' @param cache_dir Optional string for path to directory in which to
#'   cache results.
#' @param per_page Integer for how to chunk fetching of results.
#' @param max_pages Integer for maximum number of pages to fetch. `NULL`
#'   indicates all pages.
#' @param document_id Optional integer for retrieving results for a specific
#'   document.
#' @param without_images Logical for disabling temporary links to images,
#'   thereby considerably reducing time to fetch results.
#' @param without_uploads Logical for disabling information about uploads
#'   (`pages` element), thereby marginally reducing time to fetch results.
#' @param from Optional integer, interpreted as a Unix timestamp, for retrieving
#'   results created or updated after a given date and time.
#'
#' @export
psio_get_entries = function(
    survey_id, cache_dir = NULL, per_page = 200L, max_pages = 5L,
    document_id = NULL, without_images = TRUE, without_uploads = TRUE,
    from = NULL) {
  assert_int(survey_id, lower = 1L)
  assert_string(cache_dir, null.ok = TRUE)
  assert_int(per_page, lower = 1L, upper = 1000L)
  assert_int(max_pages, lower = 1L, null.ok = TRUE)
  assert_int(document_id, lower = 1L, null.ok = TRUE)
  assert_flag(without_images)
  assert_flag(without_uploads)
  assert_int(from, lower = 0L, null.ok = TRUE)

  query_args = list(
    document_id = document_id, without_images = as.integer(without_images),
    without_uploads = as.integer(without_uploads), from = from)
  psio_get_data(
    survey_id, 'entries', cache_dir = cache_dir, per_page = per_page,
    max_pages = max_pages, query_args = query_args)
}

#' Get fields
#'
#' @param entries Result returned by [psio_get_entries()].
#' @param recoding `data.frame` for recoding field names.
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
  setkeyv(fields, 'field_id')
}

#' Get answers
#'
#' @param entries Result returned by [psio_get_entries()].
#' @param format String for whether to return simple or detailed results.
#'
#' @export
psio_get_answers = function(entries, format = c('simple', 'detailed')) {
  assert_list(entries)
  format = match.arg(format)

  field_ids = map_int(entries[[1L]]$answers, \(x) x$field$id)
  cols = setdiff(names(entries[[1L]]), c('answers', 'notes', 'pages'))
  answers_base = rbindlist(
    map(entries, \(entry) entry[cols]), idcol = 'entry_row')
  key_cols = c('entry_row', 'public_id', 'internal_id')

  if (format == 'simple') {
    answers_list = map(entries, \(entry) {
      as.list(map_chr(entry$answers, 'text', .default = NA))
    })

    answers_simp = rbindlist(answers_list)
    setnames(answers_simp, paste0('field_id_', field_ids))
    answers = cbind(answers_base, answers_simp)

  } else {
    cols = setdiff(names(entries[[1L]]$answers[[1L]]), c('field', 'array'))
    answers_list = map(entries, \(entry) {
      rbindlist(map(entry$answers, \(x) x[cols]), idcol = 'answer_row')
    })

    answers_deet = rbindlist(answers_list, idcol = 'entry_row')
    set(answers_deet, j = 'field_id',
        value = field_ids[answers_deet$answer_row])
    setnames(answers_deet, c('id', 'text'), \(x) paste0('answer_', x))

    answers = data.table::merge.data.table(
      answers_base, answers_deet, by = 'entry_row')
    key_cols = c(key_cols, c('answer_row', 'answer_id'))
  }

  set_to_posix(answers)
  setkeyv(answers, key_cols)
}

#' Get questions
#'
#' @param survey_id Integer for survey id.
#'
#' @export
psio_get_questions = function(survey_id) {
  assert_int(survey_id, lower = 1L)
  resp = req_start() %>%
    req_url_path_append(survey_id, 'entries', 'questions') %>%
    req_finish()
  questions = rbindlist(
    resp, use.names = TRUE, fill = TRUE, check_list_cols = TRUE)
  setnames(questions, c('id', 'name'), \(x) paste0('field_', x))
  setkeyv(questions, 'field_id')
}
