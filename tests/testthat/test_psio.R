test_that('psio_set_api_key error', {
  expect_error(psio_set_api_key('this_file_no_exist.cya'))
})

test_that('psio_get_api_key ok', {
  expect_no_error(psio_get_api_key())
})

test_that('psio_get_user ok', {
  expect_list(psio_get_user())
})

test_that('psio_set_team ok', {
  withr::local_envvar(RPAPERSURVEY_TEAM = '')
  expect_no_error(psio_set_team(1929L))
})

test_that('psio_get_surveys ok all', {
  expect_data_table(psio_get_surveys())
})

test_that('psio_get_surveys ok one', {
  expect_data_table(psio_get_surveys(survey_id), nrows = 1L)
})

test_that('psio_get_versions ok', {
  expect_data_table(psio_get_versions(survey_id))
})

test_that('psio_get_reviews ok', {
  reviews = psio_get_reviews(survey_id, cache_dir = cache_dir, per_page = 2L)
  expect_data_table(reviews)
})

test_that('psio_get_documents ok', {
  expect_data_table(psio_get_documents(survey_id))
})

test_that('psio_get_questions ok', {
  expect_data_table(psio_get_questions(survey_id))
})

test_that('psio_get_entries ok', {
  entries = psio_get_entries(
    survey_id, cache_dir = cache_dir, per_page = 1L, max_pages = 2L)
  expect_class(entries, 'psio_entries')
  expect_no_error(print(entries))
})

