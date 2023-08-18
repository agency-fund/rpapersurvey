library('data.table')
library('lintr')
library('rex')

########################################

doubleQuotesLinter = function(sourceFile) {
  content = sourceFile$full_parsed_content
  strIdx = which(content$token == 'STR_CONST')
  squoteMatches = which(re_matches(
    content[strIdx, 'text'],
    rex(start, double_quote, any_non_single_quotes, double_quote, end)))

  lapply(
    squoteMatches,
    function(id) {
      with(content[strIdx[id], ], {
        line = sourceFile$file_lines[line1]
        col2 = if (line1 == line2) col2 else nchar(line)
        Lint(
          filename = sourceFile$filename,
          line_number = line1,
          column_number = col1,
          type = 'style',
          message = 'Only use single-quotes.',
          line = line,
          ranges = list(c(col1, col2)))
      })
    })
}


getLintIgnore = function(path = 'lint_ignore.csv') {
  lintIgnore = if (file.exists(path)) {
    fread(path)
  } else {
    data.table(
      filename = as.character(), line_number = as.integer(),
      message = as.character(), line = as.character())
  }
  return(lintIgnore)
}


getLintDt = function(lintsFound, repository = NULL, branch = NULL) {
  if (is.null(repository)) {
    dirIndexes = gregexpr('/', getwd())[[1L]]
    repository = substr(getwd(), dirIndexes[length(dirIndexes)] + 1L, nchar(getwd()))
  }

  if (is.null(branch)) branch = 'main'

  lfDt = unique(
    as.data.table(lintsFound), by = c('filename', 'line_number', 'message'))

  lfDt[, lint_link := sprintf(
    'https://github.com/hugheylab/%s/blob/%s/%s#L%s',
    repository, branch, filename, line_number)]
  lfDt[, line := trimws(line)]
  setorder(lfDt, filename, line_number)

  lintIgnore = getLintIgnore()
  lintIgnoreNotFound = lintIgnore[!lfDt, on = colnames(lintIgnore)]
  if (nrow(lintIgnoreNotFound) > 0) {
    warning('lint_ignore.csv contains lines not found in the current code.')
  }
  lfDt = lfDt[!lintIgnore, on = colnames(lintIgnore)]

  newlineEsc = ' \r\n'
  lfDt[, format_line := sprintf(
    '%s. %s line %s: %s (%s) %s    ```r %s    %s  %s    ```',
    .I, filename, line_number, message, lint_link, newlineEsc, newlineEsc, line, newlineEsc)]
  return(lfDt)
}


getFormattedIssueStr = function(lfDt) {
  newlineEsc = ' \r\n'
  issueStr = paste0(lfDt$format_line, collapse = newlineEsc)
  fileName = 'temp_csv.csv'
  fwrite(lfDt[, .(filename, line_number, message, line)], fileName)
  tempCsvStr = readChar(fileName, file.info(fileName)$size)
  unlink(fileName)

  issueStr = paste0(
    issueStr, newlineEsc,
    'To have lintr ignore any of these issues, add the appropriate lines of ',
    'those shown below to lint_ignore.csv at the top-level of the repository:',
    newlineEsc, '```', newlineEsc, tempCsvStr, newlineEsc, '```')

  lintIgnore = getLintIgnore()
  lintIgnoreNotFound = lintIgnore[!lfDt, on = colnames(lintIgnore)]

  if (nrow(lintIgnoreNotFound) > 0) {
    lintIgnoreNotFound[, format_line := sprintf(
      '%s line %s: %s \r\n    ```r \r\n    %s  \r\n    ```',
      filename, line_number, message, line)]
    notFoundStr = paste0(lintIgnoreNotFound$format_line, collapse = '\r\n')

    issueStr = sprintf(
      '%s%sWarning, lint_ignore.csv contains lines not found in the current code: %s%s',
      issueStr, newlineEsc, newlineEsc, notFoundStr)
  }

  return(issueStr)
}

########################################

if (!exists('repository')) {
  dirIndexes = gregexpr('/', getwd())[[1]]
  repository = substr(getwd(), dirIndexes[length(dirIndexes)] + 1, nchar(getwd()))
}

if (!exists('branch')) branch = 'main'

newDefaults = linters_with_defaults(
  assignment_linter = NULL,
  commented_code_linter = NULL,
  cyclocomp_linter = NULL,
  double_quotes_linter = Linter(doubleQuotesLinter),
  line_length_linter(120),
  single_quotes_linter = NULL,
  object_usage_linter = NULL)

lintsFound = lint_dir(
  linters = newDefaults, pattern = rex('.', or(one_of('Rr'), 'Rmd'), end))
lintsFound
