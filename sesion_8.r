# Relationship between categorical variables
library(tidyverse)
library(ggmosaic)

# Read the data
bike_buyers <- read_csv("datasets/bike_buyers.csv")
# Show data (only for analysis)
head(bike_buyers)

# Create a contingency table
# group_by() + summarise() + spread()
# group_by() groups the data by the variables specified
# summarise() creates a new table with columns computed as the function specified
# for example, n() counts the number of rows, due to the group_by() the number of rows is computed for each group
contingent_table <- bike_buyers %>% 
    group_by(`Marital Status`, `Education`) %>%
    summarise(n = n())

# Only to video purposes
contingent_table

# Only to avoid NA values for video purposes
contingent_table <- contingent_table %>% 
    filter(!is.na(`Marital Status`))

# Generate the contingency table using spread()
# spread() creates a new column for each value of the variable specified
contingent_table <- contingent_table %>% 
    spread(`Marital Status`, n)

# Only to video purposes
contingent_table

# Summary table. Note we don't store the table into a variable
# There are not "variable_name <- " at the beginning
# Only for video purposes
contingent_table %>% summarise(
    Married = sum(Married),
    Single = sum(Single),
    Total = sum(`Single`, `Married`),
    `% Married` = Married / Total,
    `% Single` = Single / Total,
)

# Not marginal table with percentages
# Also, this table is not stored into a variable
# Only for video purposes
contingent_table %>% mutate(
    `% Married` = Married / (Married + Single),
    `% Single` = Single / (Married + Single),
)

# Hypothesis test
# Note that we transform the table into a matrix because prop.test() requires a matrix
prop.test(as.matrix(contingent_table %>% select(Married, Single)))

# rename column `Marital Status` to `Marital_Status`
# mosaic() requires the column names without spaces
bike_buyers <- bike_buyers %>% 
    rename(
        `Marital_Status` = `Marital Status`
    )

# Mosaic plot
mosaic_example <- bike_buyers %>% 
    filter(!is.na(Marital_Status)) %>%
    ggplot() +
    geom_mosaic(aes(x = product(Marital_Status, Education), fill = Marital_Status)) +   
    labs(y="Marital Satus", x="Education")

mosaic_example
