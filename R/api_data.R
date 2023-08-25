#' Get documents
#'
#' @param survey_id Integer for survey id.
#' @param per_page Integer for how to chunk fetching of results.
#'
#' @return A `data.table` having one row per document.
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
#' An entry corresponds to a completed survey.
#'
#' @param survey_id Integer for survey id.
#' @param cache_dir Optional string for path to directory in which to cache
#'   results.
#' @param per_page Integer for how to chunk fetching of results.
#' @param max_pages Integer for maximum number of pages to fetch. `NULL`
#'   indicates all pages, i.e., all results.
#' @param document_id Optional integer for retrieving results for a specific
#'   document.
#' @param without_images Logical for disabling temporary links to images,
#'   thereby considerably reducing time to fetch results.
#' @param without_uploads Logical for disabling information about uploads
#'   (`pages` element), thereby marginally reducing time to fetch results.
#' @param from Optional integer, interpreted as a Unix timestamp, for retrieving
#'   results created or updated after a given date and time.
#'
#' @return A list having one element per entry.
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
  entries = get_data(
    survey_id, 'entries', cache_dir = cache_dir, per_page = per_page,
    max_pages = max_pages, query_args = query_args)
  class(entries) = 'psio_entries'
  entries
}

#' @export
print.psio_entries = function(x, ...) {
  cat(length(x), 'entries for survey_id', x[[1L]]$survey_id, '\n\n')
  cat('First entry, excluding `answers`:\n')
  print(x[[1L]][!(names(x[[1L]]) %in% 'answers')])
  cat('First answer of first entry:\n')
  print(x[[1L]]$answers[[1L]])
  invisible(x)
}

#' Extract fields from entries
#'
#' A field corresponds to an individual question in a survey, which
#' distinguishes the results returned by this function and by
#' [psio_get_questions()].
#'
#' @param entries Result returned by [psio_get_entries()].
#' @param recoding Optional `data.frame` for recoding field names. If not
#'   `NULL`, must include columns `field_id` and `field_name_new`.
#'
#' @return A `data.table` having one row per field.
#'
#' @export
psio_get_fields = function(entries, recoding = NULL) {
  assert_class(entries, 'psio_entries')
  assert_data_frame(recoding, null.ok = TRUE)
  if (!is.null(recoding)) {
    musty = c('field_id', 'field_name_new')
    assert_names(colnames(recoding), type = 'unique', must.include = musty)
    assert_integerish(recoding$field_id, unique = TRUE)
  }

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

#' Extract answers from entries
#'
#' As entries come from the Data API, this function returns slightly different
#' information than [psio_get_reviews()], which uses the Review API.
#'
#' @param entries Result returned by [psio_get_entries()].
#'
#' @return A list of three `data.table`s having one row per entry, per field,
#' and per option, respectively.
#'
#' @export
psio_get_answers = function(entries) { #, per = c('entry', 'field', 'option')) {
  assert_class(entries, 'psio_entries')
  # per = match.arg(per)

  cols = setdiff(names(entries[[1L]]), c('answers', 'notes', 'pages'))
  answers_base = rbindlist(
    map(entries, \(entry) entry[cols]), idcol = 'entry_row')
  set_to_posix(answers_base)
  field_ids = map_int(entries[[1L]]$answers, \(x) x$field$id)

  answers = list()
  answers$per_entry = get_answers_per_entry(entries, answers_base, field_ids)
  answers$per_field = get_answers_per_field(entries, answers_base, field_ids)
  answers$per_option = get_answers_per_option(entries, answers_base)

  key_cols = c(
    'entry_row', 'public_id', 'internal_id',
    'answer_row', 'answer_id', 'option_id')
  map(answers, \(x) setkeyv(x, intersect(colnames(x), key_cols)))
  answers
}

#' Get questions
#'
#' Here a question could correspond to a group of single-choice fields, which
#' distinguishes the results returned by this function and by
#' [psio_get_fields()].
#'
#' @param survey_id Integer for survey id.
#'
#' @return A `data.table` having one row per question.
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
