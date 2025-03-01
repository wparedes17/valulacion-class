library(tidyverse)

lr <- read_delim(
    file="datasets/left_right.csv",
    delim=" ",
    col_names=FALSE
  )

lf_df <- tibble(
    sujeto = c(lr$X1, lr$X4),
    right = c(lr$X2, lr$X5),
    left = c(lr$X3, lr$X6)
  )

lf_df <- na.omit(lf_df)

t.test(lf_df$right, lf_df$left, paired = TRUE, alternative = "less")



####


ssha <- tibble(
  women = c(154,109,137,115,152,140,154,178,101,103,126,126,137,165,165,129,200,148, NA, NA),
  men = c(108,140,114,91,180,115,126,92,169,146,109,132,75,88,113,151,70,115,187,104)
)

ssha %>% ggplot(aes(x=men)) +
  geom_histogram(bins=5)

mean(ssha$men, na.rm=TRUE)

ssha %>% ggplot(aes(x=women)) +
  geom_histogram(bins=5)

mean(ssha$women, na.rm=TRUE)

t.test(
    ssha$men,
    ssha$women,
    paired=FALSE,
    alternative="less",
    var.equal=FALSE,
    na.rm=TRUE,
    conf.level=0.90
)