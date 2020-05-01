anonymize <- c()
column <- c("Name", "Address", "GPA")
checkPosition <- c(1, 2)

for (i in 1:length(column)) {
  if (i %in% checkPosition) {
    anonymize[i] <- TRUE
  }
  else {
    anonymize[i] <- FALSE
  }
}

template <- data.frame(column, anonymize)
write.csv(template, "D:\\College Stuff\\Spring 2020\\CIS 499\\Anonytics Database Files\\testOne.csv", row.names = FALSE)