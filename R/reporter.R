#' Test reporter: Simplified summary of failures and errors.
#'
#' This reporter is used to report errors and failures to students. Only
#' the testname and test information is returned. Otherwise similar to
#' SummaryReporter.
#'
#' @export
#' @export StudentReporter
#' @aliases StudentReporter
#' @keywords debugging internal
#' @param ... Arguments used to initialise class

StudentReporter <- 
  setRefClass("StudentReporter", 
              contains = "Reporter",
              fields = list(
                "failures" = "list",
                "n" = "integer",
                "has_tests" = "logical",
                "max_reports" = "numeric",
                "show_praise" = "logical"),
                
              methods = list(
                initialize = function(max_reports = Inf, ...) {
                                   max_reports <<- max_reports
                                   show_praise <<- TRUE
                                   callSuper(...)
                                 },
                
                start_context = function(desc) {
                  cat(desc, ": ")
                  },
                
                end_context = function() {
                  cat("\n")
                },
                
                start_reporter = function() {
                  failures <<- list()
                  has_tests <<- FALSE
                  n <<- 0L
                  cat("Marking assignment...\n\n")
                  },
                  
                add_result = function(result) {
                  has_tests <<- TRUE
                  if (result$skipped) {
                    cat(testthat:::colourise("S", "skipped"))
                    return()
                  }
                  if (result$passed) {
                    cat(testthat:::colourise(".", "passed"))
                    return()
                  }
                  
                  failed <<- TRUE
                  
                  if (n + 1 > length(testthat:::labels) || n + 1 > max_reports) {
                    cat(testthat:::colourise("F", "error"))
                    } else {
                      n <<- n + 1L
                      result$test <- if (is.null(test)) "(unknown)" else test
                      failures[[n]] <<- result
                      cat(testthat:::colourise(testthat:::labels[n], "error"))
                    }
                  },
                
                end_reporter = function() {
                  charrep <- function(char, times) {
                    sapply(times, function(i) paste0(rep.int(char, i), collapse = ""))
                  }
                  
                  if (n == 0) {
                    cat("\n")
                    } else {
                      label <- testthat:::labels[seq_len(n)]
                      type <- ifelse(sapply(failures, "[[", "error"), "Error", "Failure")
                      tests <- vapply(failures, "[[", "test", FUN.VALUE = character(1))

                      header <- paste0(label, ". ", type, ": ", tests, " ")
                      linewidth <- ifelse(nchar(header) > getOption("width"),0,getOption("width") - nchar(header))
                      line <- charrep("-", linewidth )
                      message <- vapply(failures, "[[", "failure_msg", FUN.VALUE = character(1))
                      for (i in seq_along(message)){
                        # message <- paste("Info:", unlist(lapply(strsplit(message, split = "\n"), function(X) X[length(X)])))                         
                        if(type[i] == "Failure") {
                          temp_mes <- strsplit(message[i], split = "\n")[[1]]
                          message[i] <- temp_mes[length(temp_mes)]
                        }
                      }
                      reports <- paste0(
                        testthat:::colourise(header, "error"), line, "\n",
                        message, "\n")
                      cat("\n", reports, sep = "\n")
                    }
                }
                )
)

