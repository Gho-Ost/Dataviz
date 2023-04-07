sets <- read.csv("C:\\Users\\Komputer\\OneDrive\\Pulpit\\rebrickable\\sets.csv", header=T)
themes <- read.csv("C:\\Users\\Komputer\\OneDrive\\Pulpit\\rebrickable\\themes.csv", header=T)
# Number of parts per year
average_parts_per_year <- aggregate(num_parts ~ year, sets, mean)
# Number of sets per year
num_sets_per_year <- table(sets$year)
num_sets_per_year_df <- as.data.frame(num_sets_per_year)
colnames(num_sets_per_year_df) <- c("year", "num_sets")

# Plots
# Plot for number of sets a year

# Create a vector of selected years
selected_years <- c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)

# Create the plot

# First Plot
# No Regression line
ggplot(num_sets_per_year_df, aes(x = year, y = num_sets,group=1)) +
  geom_line() +
  scale_x_discrete(breaks = selected_years) +
  xlab("Year") +
  ylab("Number of Sets") +
  ggtitle("Number of Sets per Year")

# Create a new dataframe without the 2023 year column
num_sets_per_year_df_new <- head(num_sets_per_year_df,-1)

exp_model <- lm(log(num_sets_per_year_df_new$num_sets)~ as.integer(num_sets_per_year_df_new$year))
summary(exp_model)


#Second plot
#Exponential Regression Line added
ggplot(num_sets_per_year_df_new, aes(x = year, y = num_sets, group=1)) +
  geom_line() +
  geom_smooth(method = "lm", formula = y ~ exp(coef(exp_model)[1] + coef(exp_model)[2] * x), se = FALSE) +
  scale_x_discrete(breaks = selected_years) +
  xlab("Year") +
  ylab("Number of Sets") +
  ggtitle("Number of Sets per Year")


