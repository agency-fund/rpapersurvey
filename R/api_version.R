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
