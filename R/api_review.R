#' Get answers for review
#'
#' This function uses the Review API, which formats results slightly differently
#' than the Data API used by [psio_get_entries()] and thus [psio_get_answers()].
#'
#' @param survey_id Integer for survey id.
#' @param cache_dir Optional string for path to directory in which to cache
#'   results.
#' @param per_page Integer for how to chunk fetching of results.
#' @param max_pages Integer for maximum number of pages to fetch. `NULL`
#'   indicates all pages, i.e., all results.
#' @param sort_by String for field by which to sort results.
#' @param sort_order String for order in which to sort results.
#' @param filters Optional list for returning results meeting certain criteria.
#'
#' @return A `data.table` having one row per answer. Does not include prefilled
#' responses or responses collected via web form.
#'
#' @export
psio_get_reviews = function(
    survey_id, cache_dir = NULL, per_page = 200L, max_pages = 5L,
    sort_by = c(
      'answer_id', 'entry_id', 'upload_id', 'question_id',
      'verifiable_id', 'quality', 'answer', 'accepted', 'updated_at'),
    sort_order = c('DESC', 'ASC'), filters = NULL) {
  assert_int(survey_id, lower = 1L)
  assert_string(cache_dir, null.ok = TRUE)
  assert_int(per_page, lower = 1L, upper = 1000L)
  assert_int(max_pages, lower = 1L, null.ok = TRUE)
  sort_by = match.arg(sort_by)
  if (sort_by == 'answer_id') sort_by = 'id'
  sort_order = match.arg(sort_order)
  assert_list(filters, null.ok = TRUE)
  if (!is.null(filters)) {
    ok_names = c('quality', 'answer', 'accepted', 'verifiable_id')
    assert_names(names(filters), type = 'unique', subset.of = ok_names)
  }

  query_args = list(sort_by = sort_by, sort_order = sort_order)
  data = get_data(
    survey_id, 'review', cache_dir, per_page, max_pages,
    query_args = query_args, body_arg = filters, method = 'POST')
  if (length(data) == 0L) return(data.table())
  reviews = rbindlist(
    data, use.names = TRUE, fill = TRUE, check_list_cols = TRUE)
  set_to_posix(reviews)
  setnames(reviews, 'id', 'answer_id')
  # TODO: use setkeyv instead of sort_by and sort_order?
}

#' Mark answers as verified or not verified
#'
#' @param survey_id Integer for survey id.
#' @param answer_ids Vector of integers for the answer id(s), as obtainable from
#'   [psio_get_answers()] or [psio_get_reviews()].
#' @param status String for review status.
#' @param dry_run Logical for whether to do a dry run or the real deal.
#'
#' @return If `dry_run` is `TRUE`, a list returned by [httr2::req_dry_run()].
#' Otherwise, an [httr2::response()].
#'
#' @export
psio_set_reviews = function(
    survey_id, answer_ids, status = c('verified', 'not verified'),
    dry_run = TRUE) {
  assert_int(survey_id, lower = 1L)
  assert_integerish(
    answer_ids, lower = 1L, any.missing = FALSE, min.len = 1L, unique = TRUE)
  status = match.arg(status)
  status = if (status == 'verified') 'verify' else 'flag-to-verify'
  assert_flag(dry_run)

  req_start(method = 'PUT') %>%
    req_url_path_append(survey_id, 'bulk', status) %>%
    req_body_json(list(ids = answer_ids)) %>%
    req_finish(dry_run = dry_run, json_response = FALSE)
}

#' Update an answer to a particular question.
#'
#' @param survey_id Integer for survey id.
#' @param answer_id Integer for the answer id, as obtainable from
#'   [psio_get_answers()] or [psio_get_reviews()].
#' @param answer Value for the new answer. Depending on the type of question,
#'   could be a string, integer, or list.
#' @param dry_run Logical for whether to do a dry run or the real deal.
#'
#' @return If `dry_run` is `TRUE`, a list returned by [httr2::req_dry_run()].
#' Otherwise, an [httr2::response()].
#'
#' @export
psio_set_answer = function(survey_id, answer_id, answer, dry_run = TRUE) {
  assert_int(survey_id, lower = 1L)
  assert_int(answer_id, lower = 1L)
  assert(check_string(answer), check_int(answer), check_list(answer))
  assert_flag(dry_run)

  req_start(method = 'PUT') %>%
    req_url_path_append(survey_id, 'verify', answer_id) %>%
    req_body_json(list(answer = answer)) %>%
    req_finish(dry_run = dry_run)
}
