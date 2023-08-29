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

test_that('psio_get_surveys ok', {
  expect_data_table(psio_get_surveys())
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

test_that('psio_get_entries and friends ok', {
  entries = psio_get_entries(
    survey_id, cache_dir = cache_dir, per_page = 1L, max_pages = 2L)
  expect_class(entries, 'psio_entries')
  expect_no_error(print(entries))

  entries_cached = psio_get_entries(
    survey_id, cache_dir = cache_dir, per_page = 1L, max_pages = 2L)
  expect_class(entries_cached, 'psio_entries')

  expect_data_table(psio_get_fields(entries))
  recoding = data.table(
    field_id = c(380954L, 382917L), field_name_new = c('where_from', 'how_do'))
  expect_data_table(psio_get_fields(entries, recoding))

  answers = psio_get_answers(entries)
  expect_list(answers)
  lapply(answers, \(x) expect_data_table(x))
})

test_that('psio_set_reviews ok', {
  expect_list(psio_set_reviews(
    survey_id, answer_ids = 11L, status = 'not verified', dry_run = TRUE))

})

test_that('psio_set_answers ok', {
  expect_list(psio_set_answer(
    survey_id, answer_id = 11L, answer = 'Rogers', dry_run = TRUE))
})
