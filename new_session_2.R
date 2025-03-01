library(tidyverse)

population <- tibble(
    student = 1:1000,
    note = rnorm(1000, 6.5, 2)
)

mean_population <- mean(population$note)



for(i in 1:100){
  my_subjects <- sample(1:1000, 20)
  my_sample <- subset(population, student %in% my_subjects)
  mean_sample <- mean(my_sample$note)
  my_test <- t.test(my_sample$note, conf.level = 0.95)
  ci_lower <- my_test$conf.int[1]
  ci_upper <- my_test$conf.int[2]
  is_in_ci <- ci_lower <= mean_population && mean_population <= ci_upper
  if (is_in_ci) {
      print("TRUE")
  } else {
      print("FALSE")
      break
  }
}




