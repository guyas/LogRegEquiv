#' Student Performance Data Set
#'
#'
#' Data from a student achievement in secondary education of two Portuguese
#' schools. Full attribute description could be found in the source webpage.
#'
#' The data used is taken from the Student Performance Data. The original data
#' consists of 30 covariates (13 binary, 11 ordinal, 4 categorical, 2
#' numerical) and a numerical output variable indicating the students final
#' grade in Portuguese Language course.
#'
#' The  data was split by gender (F/M) \eqn{n_f=383, n_m=266}. The target
#' variable \code{G3} was converted to binary, \code{final_fail} which
#' indicates the cases where \code{G3 < 10}.
#'
#' Next, each sub-population was divided into training and testing data, using
#' a 4:1 ratio.
#'
#' @docType data
#'
#' @references P. Cortez and A. Silva. Using Data Mining to Predict Secondary
#' School Student Performance. In A. Brito and J. Teixeira Eds., Proceedings
#' of 5th FUture BUsiness TEChnology Conference (FUBUTEC 2008) pp. 5-12,
#' Porto, Portugal, April, 2008, EUROSIS, ISBN 978-9077381-39-7.
#'
#' @source \url{https://archive.ics.uci.edu/ml/datasets/student+performance}
#'
#' @seealso \url{http://www3.dsi.uminho.pt/pcortez/student.pdf}
#'
"ptg_stud_data"

#' Student Performance Data Set - female training data
#'
#' @seealso \code{ptg_stud_data}
#'
"ptg_stud_f_train"

#' Student Performance Data Set - female testing data
#'
#' @seealso \code{ptg_stud_data}
#'
"ptg_stud_f_test"

#' Student Performance Data Set - male training data
#'
#' @seealso \code{ptg_stud_data}
#'
"ptg_stud_m_train"

#' Student Performance Data Set - male testing data
#'
#' @seealso \code{ptg_stud_data}
#'
"ptg_stud_m_test"
