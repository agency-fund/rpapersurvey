#' Get entry responses for review
#'
#' Similar to getting entries via the Data API but more compact, with extra
#' metadata used when verifying/reviewing answers.
#'
#' @param survey_id Integer indicating survey id.
#' @param cache_dir Path to directory in which to cache results.
#' @param per_page Integer indicating how to chunk responses.
#' @param max_pages Integer indicating maximum number of pages to fetch.
#' @param ... Other parameters passed to the API.
#'
#' @export
psio_get_reviews = function(
    survey_id, cache_dir = NULL, per_page = 200L, max_pages = 5L, ...) {
  assert_int(survey_id, lower = 1L)
  assert_string(cache_dir, null.ok = TRUE)
  assert_int(per_page, lower = 1L)
  assert_int(max_pages, lower = 1L, null.ok = TRUE)
  data = psio_get_data(
    survey_id, 'review', cache_dir, per_page, max_pages, method = 'POST', ...)
  reviews = rbindlist(
    data, use.names = TRUE, fill = TRUE, check_list_cols = TRUE)
  setnames(reviews, 'id', 'answer_id')[]
}

#' Mark responses as verified or not verified
#'
#' @param survey_id Integer indicating survey id.
#' @param answer_ids Vector of integers indicating the answer id(s), as seen in
#'   [psio_get_answers()] ('detailed') or [psio_get_reviews()].
#' @param status String indicating review status.
#' @param dry_run Logical indicating whether to do a dry run or the real deal.
#'
#' @export
psio_set_reviews = function(
    survey_id, answer_ids, status = c('verified', 'not verified'),
    dry_run = TRUE) {
  assert_int(survey_id, lower = 1L)
  assert_integer(answer_ids, lower = 1L, any.missing = FALSE, min.len = 1L)
  status = match.arg(status)
  assert_flag(dry_run)

  stat = if (status == 'verified') 'verify' else 'flag-to-verify'
  req_start() %>%
    req_url_path_append(survey_id, 'bulk', stat) %>%
    req_body_json(list(ids = answer_ids)) %>%
    req_finish(method = 'PUT', dry_run = dry_run, json_response = FALSE)
}

#' Update an answer to a particular question.
#'
#' @param survey_id Integer indicating survey id.
#' @param answer_id Integer indicating the answer id as seen in
#'   [psio_get_answers()] ('detailed') or [psio_get_reviews()].
#' @param answer Value indicating the new answer. Depending on the type of
#'   question, could be a string, integer, or list.
#' @param dry_run Logical indicating whether to do a dry run or the real deal.
#'
#' @export
psio_set_answer = function(survey_id, answer_id, answer, dry_run = TRUE) {
  assert_int(survey_id, lower = 1L)
  assert_int(answer_id, lower = 1L)
  assert_flag(dry_run)
  req_start() %>%
    req_url_path_append(survey_id, 'verify', answer_id) %>%
    req_body_json(list(answer = answer)) %>%
    req_finish(method = 'PUT', dry_run = dry_run)
}
