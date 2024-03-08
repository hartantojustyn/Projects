library(dplyr)
library(tidyverse)
library(ggplot2)
install.packages("GGally")
library(GGally)
library(polycor)

raw_data$ATTEND <- factor(raw_data$ATTEND, levels = 1:3, labels = c("always", "sometimes", "never"))
raw_data$GENDER <- factor(raw_data$GENDER,levels = c(1,2), labels =c("female","male"))
raw_data$GRADE <- factor(raw_data$GRADE,levels = c(0,1,2,3,4,5,6,7), labels =c("Fail","DD", "DC","CC","CB", "BB","BA", "AA"), ordered = TRUE)
raw_data$FATHER_EDU <- factor(raw_data$FATHER_EDU, levels = 1:6, labels = c("primary school", "secondary school", "high school", "university", "MSc.", "Ph.D."))
raw_data$CUML_GPA <- factor(raw_data$CUML_GPA, levels = 1:5, labels = c("<2.00", "2.00-2.49", "2.50-2.99", "3.00-3.49", "above 3.49"))
str(raw_data)

#polycor
polycor_matrix <- polychor(raw_data$CUML_GPA, raw_data$ATTEND)
print(polycor_matrix)

#chisquare
rawr <- table(raw_data$ATTEND, raw_data$CUML_GPA)
chisq <- chisq.test(rawr)
print(chisq)

#univariate attend
ggplot(raw_data, aes(x = ATTEND, fill = ATTEND)) +
  geom_bar(fill="red") +
  labs(title="Barchart for ATTEND", x="ATTEND", y="Count")

data_sum <- raw_data %>%
  group_by(ATTEND) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
data_sum
ggplot(data = data_sum,
       aes(x = "",y = percent, fill = ATTEND)) +
  geom_bar(stat = "identity", width = 1, color = NA) + # set color to NA to remove piechart outline
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(percent),"%")),
            position = position_stack(vjust = 0.5),
            size = 6) +
  labs(title = "Attendance Pie Chart",
       fill = "Attendance Frequency") +
  scale_fill_manual(values = c("#DD4423","#A32323")) +
  theme_void() + #biar ga keluar 0/100 diluar lingkaran 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 16, 
                                  hjust = 0.5, 
                                  vjust = 0.5, 
                                  margin = margin(10, 10, 10, 10)),
        legend.title = element_text(size = 12))


#biavariate

library(ggplot2)
library(dplyr)

# Assuming raw_data contains the necessary columns

# Calculate counts for each combination of ATTEND and CUML_GPA
count_data <- raw_data %>%
  count(ATTEND, CUML_GPA) %>%
  group_by(ATTEND) %>%
  mutate(percentage = prop.table(n) * 100)

ggplot(data = count_data, aes(x = ATTEND, y = n, fill = CUML_GPA)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5) + # Add count labels
  labs(title = "Bivariate Barchart for Student Attendance and Cumulative GPA", x = "ATTEND", y = "Count") +
  facet_grid(~ CUML_GPA)



#multivariate

library(ggplot2)

library(ggplot2)

ggplot(raw_data, aes(x = CUML_GPA, fill = GENDER)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=0.5)) + # Add count labels in the middle
  labs(title = "Attendance and Gender by Cumulative GPA", x = "Cumulative GPA", y = "Count") +
  facet_wrap(~ ATTEND, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(tidymodels)

library(ggplot2)

# Assuming 'result_df' contains the Actual and Predicted values

# Create a scatterplot comparing Actual vs. Predicted values
ggplot(predictions, aes(x = .pred_class)) +
  geom_bar() +  # Add a diagonal line for reference
  labs(title = "Actual vs. Predicted Values", x = "Predicted", y = "Actual")


datalinear <- mutate_all(raw_data, as.numeric) 
processed_data <- datalinear %>%
  select(-STUDENTID)

linear_model <- linear_reg() %>% 
  # Set the model engine
  set_engine('lm') %>% 
  # Set the model mode
  set_mode('regression')

# Fit the model using the training data
lm_fit <- linear_model %>% 
  fit(CUML_GPA ~ ATTEND + GENDER,
      data = processed_data)

lm_fit

# Calculate R-squared
library(yardstick)

# Calculate metrics
rsq_value <- rsq(lm_fit)
mse_value <- mse(lm_fit)
rmse_value <- sqrt(mse_value)

# Print the metrics
cat("R-squared:", rsq_value, "\n")
cat("Mean Squared Error:", mse_value, "\n")
cat("Root Mean Squared Error:", rmse_value, "\n")



tidy(lm_fit)

CUML_PREDICTION <- predict(lm_fit, new_data=processed_data)
CUML_PREDICTION


cuml_result <- processed_data %>% 
  select(CUML_GPA, ATTEND, GENDER) %>% 
  bind_cols(CUML_PREDICTION)

view(cuml_result)

library(yardstick)
lm_fit <- linear_model %>% 
  fit(CUML_GPA ~ ATTEND + AGE, data = processed_data)

# Make predictions on the test set
predictions <- predict(lm_fit, new_data = test_data)
predictions

# Calculate R-squared
rsq_value <- cor(predictions$.pred, test_data$CUML_GPA)^2

# Calculate Mean Squared Error
mse_value <- mean((predictions$.pred - test_data$CUML_GPA)^2)

# Calculate Root Mean Squared Error
rmse_value <- sqrt(mse_value)

# Print the metrics
cat("R-squared:", rsq_value, "\n")
cat("Mean Squared Error:", mse_value, "\n")
cat("Root Mean Squared Error:", rmse_value, "\n")

plot(predictions)
