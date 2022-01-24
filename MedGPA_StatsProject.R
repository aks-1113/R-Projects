medgpa <- read.csv("/Users/akshayraju/Downloads/MedGPA.csv", header = TRUE)
med_gpa <- data.frame(medgpa)
med_gpa$GPA

avg <- mean(med_gpa$GPA)
avg

standard_dev <- sd(med_gpa$GPA)
standard_dev

avg+standard_dev
avg-standard_dev
avg+(2*standard_dev)
avg-(2*standard_dev)

n <- nrow(med_gpa)
n

standard_error <- (standard_dev/sqrt(n))
standard_error

ninetyfive_ci <- standard_error*1.96
ninetyfive_ci

ninetynine_ci <- standard_error*2.58
ninetynine_ci
avg+ninetynine_ci
avg-ninetynine_ci

library(ggplot2)
ggplot(data = med_gpa, aes(GPA)) +
  geom_histogram(breaks=seq(2.4,4.0,by=0.2))

qplot(med_gpa$GPA,
      geom="histogram",
      binwidth=0.2,
      )

hist(med_gpa$GPA)
