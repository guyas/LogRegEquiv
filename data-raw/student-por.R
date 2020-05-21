library(usethis)

if(!file.exists("data-raw/student")){
  tmp <- tempfile(fileext = ".zip")
  download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip", tmp, quiet = TRUE)
  unzip(tmp, exdir = "data-raw/student")
  unlink(tmp)
}

ptg_stud <- read.csv(file = "data-raw/student/student-por.csv", sep = ";")
ptg_stud$final_fail <- 0 + (ptg_stud$G3 < 10)
ptg_stud_data <- ptg_stud[, -c(31:33)]
ptg_stud_f <- ptg_stud_data[ptg_stud_data$sex == "F", -2]
ptg_stud_m <- ptg_stud_data[ptg_stud_data$sex == "M", -2]

f_test_fail_size <- round(length(which(ptg_stud_f$final_fail == 1))*0.2)
m_test_fail_size <- round(length(which(ptg_stud_m$final_fail == 1))*0.2)
f_test_pass_size <- round(0.2 * nrow(ptg_stud_f)) - f_test_fail_size
m_test_pass_size <- round(0.2 * nrow(ptg_stud_m)) - m_test_fail_size


test_idx_f <- c(which(ptg_stud_f$final_fail == 1)[1:f_test_fail_size],
                which(ptg_stud_f$final_fail == 0)[1:f_test_pass_size])
test_idx_m <- c(which(ptg_stud_m$final_fail == 1)[1:m_test_fail_size],
                which(ptg_stud_m$final_fail == 0)[1:m_test_pass_size])

ptg_stud_f_test <- ptg_stud_f[test_idx_f, ]
ptg_stud_f_train <- ptg_stud_f[-test_idx_f, ]
ptg_stud_m_test <- ptg_stud_m[test_idx_m, ]
ptg_stud_m_train <- ptg_stud_m[-test_idx_m, ]

write.csv(ptg_stud_f_test, "data-raw/ptg_stud_f_test.csv", row.names = FALSE)
write.csv(ptg_stud_f_train, "data-raw/ptg_stud_f_train.csv", row.names = FALSE)
write.csv(ptg_stud_m_test, "data-raw/ptg_stud_m_test.csv", row.names = FALSE)
write.csv(ptg_stud_m_train, "data-raw/ptg_stud_m_train.csv", row.names = FALSE)


use_data(ptg_stud_data)
use_data(ptg_stud_f_test)
use_data(ptg_stud_f_train)
use_data(ptg_stud_m_test)
use_data(ptg_stud_m_train)
