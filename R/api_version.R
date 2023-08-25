#' Get survey versions
#'
#' @param survey_id Integer for survey id.
#'
#' @return A `data.table` having one row per version.
#'
#' @export
psio_get_versions = function(survey_id) {
  assert_int(survey_id, lower = 1L)
  resp = req_start() %>%
    req_url_path_append(survey_id, 'versions') %>%
    req_finish()
  versions = rbindlist(resp, use.names = TRUE, fill = TRUE)
  set_to_posix(versions)
  setnames(versions, 'id', 'version_id')
  setkeyv(versions, 'version_id')
}
