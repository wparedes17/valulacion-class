library(tidyverse)
library(gridExtra)

fatigue_crack_data <- read_csv("./datasets/fatigue_crack_data.csv")
# Checkout the data in horizontal format
glimpse(fatigue_crack_data)

# Checkout the data in vertical format
head(fatigue_crack_data)

fatigue_crack_data <- fatigue_crack_data %>%
    mutate(Subject_Index = as.factor(Subject_Index))


# fatigue function


fatigue_crack_data %>% ggplot(aes(x = Millions_of_Cycles, y = Crack_Size, color = Subject_Index)) +
    geom_point() +
    geom_line() +
    #geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Fatigue Crack Growth Data",
         x = "Millions of Cycles",
         y = "Crack Size (mm)",
         color = "Subject Index") +
    theme_minimal()


fits <- list()
for (i in unique(fatigue_crack_data$Subject_Index)) {
    data <- fatigue_crack_data %>% filter(Subject_Index == i) %>%
        select(Millions_of_Cycles, Crack_Size) 

    a_0 <- data$Crack_Size[1]
    fatigue_function <- function(Time, c, m, s) {
        term_1 <- a_0^(1 - m/2)
        term_2 <- (1 - m/2) * c * Time
        term_3 <- (s*sqrt(pi))^m

        a_t <- (term_1 + term_2 + term_3)^(2/(2 - m))
        return(a_t)
    }

    fit <- nls.multstart::nls_multstart(
        Crack_Size ~ fatigue_function(Millions_of_Cycles, c, m, s),
        data = data,
        start_lower = list(c = 0.1, m = 2.001, s = 0.1),
        start_upper = list(c = 10, m = 3.0, s = 10),
        iter = 100,
        supp_errors = 'Y'
    )

    fits[[i]] <- fit
}

summary(fits[[1]])

params_df <- data.frame(
    Subject_Index = unique(fatigue_crack_data$Subject_Index),
    c = sapply(fits, function(fit) coef(fit)[1]),
    m = sapply(fits, function(fit) coef(fit)[2]),
    s = sapply(fits, function(fit) coef(fit)[3])
)

params_df <- params_df %>%
    select(c, m, s)

params_df

# Normality check of parameters
# Check normality of each variable
p1 <- ggplot(params_df, aes(x=c)) + geom_histogram(aes(y=..density..), alpha=0.7) + 
    geom_density(color="red") + ggtitle("Variable C")
p2 <- ggplot(params_df, aes(x=m)) + geom_histogram(aes(y=..density..), alpha=0.7) + 
    geom_density(color="red") + ggtitle("Variable M")
p3 <- ggplot(params_df, aes(x=s)) + geom_histogram(aes(y=..density..), alpha=0.7) + 
    geom_density(color="red") + ggtitle("Variable S")

grid.arrange(p1, p2, p3, ncol=3)

mu_hat <- colMeans(params_df[c("c", "m", "s")])
sigma_hat <- cov(params_df[c("c", "m", "s")])

mahal_dist <- mahalanobis(params_df[c("c", "m", "s")], mu_hat, sigma_hat)
qqplot(qchisq(ppoints(length(mahal_dist)), df=3), mahal_dist,
       main="Q-Q Plot: Mahalanobis Distance vs Chi-square(3)",
       xlab="Theoretical Chi-square Quantiles",
       ylab="Sample Mahalanobis Distances")
abline(0, 1, col="red")


simulated_data <- mvtnorm::rmvnorm(n = 1000, mean = mu_hat, sigma = sigma_hat)
colnames(simulated_data) <- c("c", "m", "s")
simulated_data <- as.data.frame(simulated_data)


df <- as.data.frame(apply(simulated_data, 1, function(row){
    unname(unlist(sapply(seq(0, 0.12, by = 0.01), function(x) {
        fatigue_function(x, row[1], row[2], row[3])
    })))
}))

df$reference <- seq(0, 0.12, by = 0.01)

df

df_long <- df %>%
  # Exclude the reference column from pivoting
  pivot_longer(cols = -reference, 
               names_to = "column_index", 
               values_to = "value") %>%
  # Convert column names to numeric indices if needed
  mutate(column_index = as.numeric(gsub("V", "", column_index))) 

df_long <- df_long %>%
  mutate(column_index = column_index + 21)

colnames(df_long) <- c("Millions_of_Cycles", "Subject_Index", "Crack_Size")

df_long <- df_long %>%
  select(Subject_Index, Millions_of_Cycles, Crack_Size)

df_long <- df_long %>%
  mutate(Subject_Index = as.factor(Subject_Index))

df_final <- bind_rows(fatigue_crack_data, df_long)
df_final <- df_final %>%
  mutate(Simulated = ifelse(as.numeric(Subject_Index)>21, 1, 0))

df_final <- df_final %>% sort("Simulated", decreasing = TRUE)

# More sophisticated visualization
ggplot(df_final, aes(x = Millions_of_Cycles, y = Crack_Size, group = Subject_Index)) +
  geom_line(aes(color = factor(Simulated), linetype = factor(Simulated)), 
            alpha = 0.8, size = 0.7) +
  scale_color_manual(values = c("0" = "#D55E00", "1" = "#0072B2"),
                     labels = c("0" = "Experimental", "1" = "Simulated"),
                     name = "Data Type") +
  scale_linetype_manual(values = c("0" = "solid", "1" = "dashed"),
                        labels = c("0" = "Experimental", "1" = "Simulated"),
                        name = "Data Type") +
  labs(title = "Fatigue Crack Growth Analysis",
       x = "Millions of Cycles",
       y = "Crack Size (mm)",
       subtitle = "Comparison of Experimental and Simulated Crack Growth") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.minor = element_blank())
