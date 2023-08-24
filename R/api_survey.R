#' Get survey metadata
#'
#' @param survey_id Integer for survey id.
#'
#' @export
psio_get_surveys = function(survey_id = NULL) {
  assert_int(survey_id, lower = 1L, null.ok = TRUE)

  resp = req_start() %>%
    req_url_path_append(survey_id) %>%
    req_finish()
  if (!is.null(survey_id)) resp = list(resp)

  surveys = rbindlist(
    resp, use.names = TRUE, fill = TRUE, check_list_cols = TRUE)
  set_to_posix(surveys)
  setnames(surveys, 'id', 'survey_id')
  setkeyv(surveys, c('survey_id', 'slug'))
}
