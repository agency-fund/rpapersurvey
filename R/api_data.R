#' Get documents
#'
#' @param survey_id Integer for survey id.
#' @param per_page Integer for how to chunk fetching of results.
#'
#' @export
psio_get_documents = function(survey_id, per_page = 200L) {
  assert_int(survey_id, lower = 1L)
  assert_int(per_page, lower = 1L, upper = 1000L)
  data = get_data(survey_id, 'documents', per_page = per_page, max_pages = NULL)
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
  get_data(
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
    fields = merge.data.table(
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
#'
#' @export
psio_get_answers = function(entries) { #, per = c('entry', 'field', 'option')) {
  assert_list(entries)
  # per = match.arg(per)

  cols = setdiff(names(entries[[1L]]), c('answers', 'notes', 'pages'))
  answers_base = rbindlist(
    map(entries, \(entry) entry[cols]), idcol = 'entry_row')
  field_ids = map_int(entries[[1L]]$answers, \(x) x$field$id)

  answers = list()
  answers$per_entry = get_answers_per_entry(entries, answers_base, field_ids)
  answers$per_field = get_answers_per_field(entries, answers_base, field_ids)
  answers$per_option = get_answers_per_option(entries, answers_base)

  key_cols = c(
    'entry_row', 'public_id', 'internal_id',
    'answer_row', 'answer_id', 'option_id')
  map(answers, \(x) {
    set_to_posix(x)
    setkeyv(x, intersect(colnames(x), key_cols))
  })
  answers
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
