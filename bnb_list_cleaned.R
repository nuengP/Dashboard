
listings <- read_csv("Listings.csv")

library(dplyr)

summary(listings$price)

remove_outliers <- function(x, coef = 1.5) {
  q <- quantile(x, probs = c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - coef * iqr
  upper_bound <- q[2] + coef * iqr
  return(x[x >= lower_bound & x <= upper_bound])
}

# Apply the function to your data
rm_price <- remove_outliers(listings$price)

list_cleaned <- listings %>%
  filter(price %in% rm_price)

summary(list_cleaned$price)


# export csv file
write_csv(list_cleaned, "list_cleaned.csv")
