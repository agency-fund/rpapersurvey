## for cran submission:
# with(list(f = 'tests/testthat/scto_auth.txt'),
#      devtools::check_rhub(
#        env_vars = c(SCTO_AUTH = readChar(f, file.info(f)$size))))

library('data.table')

survey_id = 229001L
cache_dir = withr::local_tempdir(.local_envir = teardown_env())
