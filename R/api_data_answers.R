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
  if (length(entries) == 0L) return(data.table())
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

get_answers_per_entry = function(entries, answers_base, field_ids) {
  ape_list = map(entries, \(entry) {
    as.list(map_chr(entry$answers, 'text', .default = NA))
  })
  ape = rbindlist(ape_list)
  setnames(ape, paste0('field_id_', field_ids))
  ape = cbind(answers_base, ape)
}

get_answers_per_field = function(entries, answers_base, field_ids) {
  cols = setdiff(names(entries[[1L]]$answers[[1L]]), c('field', 'array'))
  apf_list = map(entries, \(entry) {
    rbindlist(map(entry$answers, \(x) x[cols]), idcol = 'answer_row')
  })
  apf = rbindlist(apf_list, idcol = 'entry_row')
  set(apf, j = 'field_id', value = field_ids[apf$answer_row])
  setnames(apf, c('id', 'text'), \(x) paste0('answer_', x))
  apf = merge.data.table(answers_base, apf, by = 'entry_row')
}

get_answers_per_option = function(entries, answers_base) {
  apo_list = map(entries, \(entry) {
    apo_now = map(entry$answers, \(answer) {
      x = unlist(answer$array)
      apo_tmp = if (length(x) == 0L) {
        data.table(NA, answer$field$id, NA, NA)
      } else {
        option_id = names(x) %||% paste0('dummy_', seq_len(length(x)))
        data.table(answer$id, answer$field$id, option_id, x)
      }
      setnames(apo_tmp, c('answer_id', 'field_id', 'option_id', 'option_name'))
    })
    apo_now = rbindlist(
      apo_now, use.names = TRUE, fill = TRUE, idcol = 'answer_row')
    set(apo_now, j = 'option_other',
        value = apo_now$option_id == 'other_selected')
    suppressWarnings(
      set(apo_now, j = 'option_id', value = as.integer(apo_now$option_id)))
  })
  apo = rbindlist(apo_list, use.names = TRUE, fill = TRUE, idcol = 'entry_row')
  apo = merge.data.table(answers_base, apo, by = 'entry_row')
}
