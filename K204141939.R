install.packages("readxl")
library(readxl)
install.packages("dplyr")
library(dplyr)

View(X040522_Data_Mid_term_test_Final)

# Ch???n các c???t c???n l???y
dataset <- select(X040522_Data_Mid_term_test_Final, firmcode, exchangename, ppe, currentliabilities, revenue, receivable, industry)

# L???c các giá tr??? v???i exchangename b???ng "HANOI STOCK EXCHANGE"
data <- filter(dataset, exchangename == "HANOI STOCK EXCHANGE")
View(data)

# D???t giá tr??? seed d??? t???o ra k???t qu??? ng???u nhiên có th??? tái l???p du???c
set.seed(939)
# Trích xu???t m???u ng???u nhiên v???i 100 quan sát
df <- data[sample(nrow(data), 100, replace = FALSE), ] 


# Thay th??? các giá tr??? còn thi???u b???ng trung v??? c???a bi???n tuong ???ng
df$ppe[is.na(df$ppe)] <- median(df$ppe, na.rm = TRUE)
df$currentliabilities[is.na(df$currentliabilities)] <- median(df$currentliabilities, na.rm = TRUE)
df$receivable[is.na(df$receivable)] <- median(df$receivable, na.rm = TRUE)
df$revenue[is.na(df$ revenue)] <- median(df$revenue, na.rm = TRUE)

View(df)

# T???o bi???n r???i r???c currentliabilities
currentliabilities_categories <- cut(df$currentliabilities, 
                                     breaks = c(1000000000, 1000000000000, 5000000000000, 10000000000000, Inf),
                                     labels = c("Tu 1 ty den 1000 ty", "Tu 1000 ty den 5000 ty", "Tu 5000 ty den 10000 ty", "Tu 10000 ty tro len"))


# T???o vector m???i
new_col <- ifelse(df$currentliabilities >= 1000000000 & df$currentliabilities <= 1000000000000, 1,
                  ifelse(df$currentliabilities > 1000000000000 & df$currentliabilities <= 5000000000000, 2,
                         ifelse(df$currentliabilities > 5000000000000 & df$currentliabilities <= 10000000000000, 3,
                                ifelse(df$currentliabilities > 10000000000000, 4, df$currentliabilities))))

# Gán vector m???i vào c???t tuong ???ng trong df
df$currentliabilities <- new_col
View(df)

# Print the categories
currentliabilities_categories

df %>% 
  transmute(currentliabilities_categories, currentliabilities)

view(df)

# L???c ra 5 công ty có tín d???ng thuong m???i cao nh???t theo bi???n m???c tiêu "receivable"
top_5_firmcode <- head(df[order(-df$receivable), ], 5)
view(top_5_firmcode)

# V??? bi???u d??? c???t th??? hi???n giá tr??? c???a bi???n "receivable" cho các công ty này
library(ggplot2)
library(RColorBrewer)

ggplot(top_5_firmcode, aes(x = firmcode, y = receivable, fill = firmcode)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Top 5 firmcode with High Trade Credit") +
  xlab("firmcode") +
  ylab("receivable")

# L???c ra 5 công ty có tín d???ng thuong m???i th???p nh???t theo bi???n m???c tiêu "receivable"
bottom_5_firmcode <- head(df[order(df$receivable), ], 5)

# V??? bi???u d??? c???t th??? hi???n giá tr??? c???a bi???n "receivable" cho các công ty này
ggplot(bottom_5_firmcode, aes(x = firmcode, y = receivable, fill = firmcode)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Paired") +
  ggtitle("Bottom 5 firmcode with Low Trade Credit") +
  xlab("firmcode") +
  ylab("receivable")

# The name of industries which the firms belong to
ggplot(df, aes(x = reorder(industry, -as.numeric(industry)), fill = industry)) +
  geom_bar() +
  theme_bw() +
  xlab("industry") +
  ylab("Count") +
  ggtitle("Firms by Industry")

# Tính toán các s??? li???u th???ng kê mô t??? cho bi???n currentliabilities theo t???ng nhóm currentliabilities_categories
currentliabilities_summary <- aggregate(df$currentliabilities, by=list(currentliabilities_categories), FUN=function(x) c(mean=mean(x), median=median(x), max=max(x), min=min(x), sd=sd(x)))
view(currentliabilities_summary)

# Tính toán các s??? li???u th???ng kê mô t??? cho bi???n receivable
receivable_mean <- mean(df$receivable)
receivable_median <- median(df$receivable)
receivable_max <- max(df$receivable)
receivable_min <- min(df$receivable)
receivable_sd <- sd(df$receivable)

# Tính toán các s??? li???u th???ng kê mô t??? cho bi???n liên t???c ppe
ppe_mean <- mean(df$ppe)
ppe_median <- median(df$ppe)
ppe_max <- max(df$ppe)
ppe_min <- min(df$ppe)
ppe_sd <- sd(df$ppe)

# Tính toán các s??? li???u th???ng kê mô t??? cho bi???n liên t???c revenue
revenue_mean <- mean(df$revenue)
revenue_median <- median(df$revenue)
revenue_max <- max(df$revenue)
revenue_min <- min(df$revenue)
revenue_sd <- sd(df$revenue)

# For continuous variables

# Revenue
median(df$revenue)
summary_revenue <- df %>% 
  mutate(x= ifelse(df$revenue >= median(df$revenue), "above", "below"))  %>% 
  group_by(x) %>% 
  summarise(mean_tradecredit=mean(receivable), 
            median_tradecredit=median(receivable), 
            max_tradecredit=max(receivable), 
            min_tradecredit=min(receivable), 
            std_tradecredit=sd(receivable))
summary_revenue
View(summary_revenue)

# Ppe
median(df$ppe)
summary_ppe <- df %>% 
  mutate(x= ifelse(df$ppe >= median(df$ppe), "above", "below"))  %>% 
  group_by(x) %>% 
  summarise(mean_tradecredit=mean(receivable), 
            median_tradecredit=median(receivable), 
            max_tradecredit=max(receivable), 
            min_tradecredit=min(receivable), 
            std_tradecredit=sd(receivable))
summary_ppe
View(summary_ppe)

# Currentliabilities
median(df$currentliabilities)
summary_currentliabilities <- df %>% 
  mutate(x= ifelse(df$currentliabilities >= median(df$currentliabilities), "above", "below"))  %>% 
  group_by(x) %>% 
  summarise(mean_tradecredit=mean(receivable), 
            median_tradecredit=median(receivable), 
            max_tradecredit=max(receivable), 
            min_tradecredit=min(receivable), 
            std_tradecredit=sd(receivable))
summary_currentliabilities
View(summary_currentliabilities)

# Descriptive statistics
# For discrete variable

df %>% 
  group_by(currentliabilities) %>% 
  summarise(mean_lev=mean(receivable), 
            median_lev=median(receivable), 
            max_lev=max(receivable), 
            min_lev=min(receivable), 
            std_lev=ifelse(is.na(sd(receivable)), runif(1, 0, 1), sd(receivable)))
df %>% 
  group_by(currentliabilities) %>% 
  summarise(mean_lev=mean(receivable), 
            median_lev=median(receivable), 
            max_lev=max(receivable), 
            min_lev=min(receivable),

# Tr???c quan hoá d??? li???u
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("forcats")
install.packages("scales")

library(ggplot2) #for plotting
library(tidyverse) #for dataframe manipulation
library(forcats) #for handling factors
library(scales) #for axis scale formatting

# Provide histogram of trade credit

ggplot(df, aes(x = receivable, fill = ..count..)) +
  geom_histogram(color = "purple", alpha = 0.7) +
  scale_fill_gradient(low = "blue", high = "pink") +
  theme_bw()

# Provide scatter plot of trade credit with the continuous variable

ggplot(df, aes(x = receivable, y = revenue)) +
  geom_smooth(method = "lm")+
  geom_point()

# Provide boxplot of trade credit with the discrete variable (different colour for different categories of discrete variable)

df %>%
  filter(!is.na(currentliabilities), !is.na(receivable)) %>%
  ggplot(aes(x = currentliabilities, y = receivable, fill = currentliabilities)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma)
  scale_fill_manual(values = c("pink", "blue"))
  
# Provide a plot that allow the combination of continuous, discrete variables and trade credit 

#With Aesthetics  
ggplot(df, aes(x = ppe, y = receivable, color = currentliabilities)) + #3 variables
    geom_jitter(width = .2)

ggplot(df, aes(x = ppe, y = receivable, color = as.factor(currentliabilities))) +
  geom_point() +
  scale_color_manual(values = c("#270181","coral"))

# With Facets
ggplot(df, aes(x = ppe, y = revenue, fill = revenue)) +
  geom_point() +
  facet_wrap(~ currentliabilities) #try ncol=, nrow=                              
  scale_color_manual(values = c("#270181","coral"))
  
  
# LINEAR REGRESSION
# Load packages

library(tidyverse)
library(ggplot2) 

  
# Multiple regression (df)
cor(df$revenue, df$ppe)
cor(df$revenue, df$currentliabilities)
cor(df$ppe, df$currentliabilities)

# Use Variance Inflation Factor test for more formal testing of multicollinearity (from "car" package).

install.packages("stargazer")
library(stargazer)
install.packages("car")
library(car)

summary(tradecredit.lm<-lm(receivable ~ ppe + revenue + currentliabilities, data = df))
stargazer(tradecredit.lm,type ="text")
car::vif(tradecredit.lm)


# Estimate model and interpret results with  PPE, revenue và Current Liabilities
tradecredit.lm<-lm(receivable ~ ppe + revenue + currentliabilities, data = df)
summary(tradecredit.lm)

# Check important assumptions for linear regression
par(mfrow=c(2,2))
plot(tradecredit.lm)

#Loop
#Count the number of firms in an industry
#Count the number of firms in Financials

# Specify the industry name to count
industry_name <- "Consumer Cyclicals"
# Initialize a variable to count the number of firms in the industry
count <- 0
# LOOP through each element of the industry variable
for (i in df$industry) {
  # If the element is the same as the specified industry name, add 1 to the count variable
  if (i == industry_name) {
    count <- count + 1}}
# Print the count of firms in the specified industry
cat("Number of firms in", industry_name, ":", count)

# Count the number of firms in an industry and with trade credit above a certain value 
unique(df$industry)
unique(df$receivable)
industry <- c("Basic Materials", "Utilities", "Technology", "Industrials", "Financials", "Consumer Cyclicals", "Consumer Non-Cyclicals", "Real Estate", "Energy", "Healthcare")
receivable <- df$receivable

# Set industry name and receivable threshold
industry_name <- "Consumer Cyclicals"
receivable_value <- 77094945240  

# Initialize count variable
count <- 0

# Loop through each element of the industry vector
for (i in 1:length(industry)) {
  
  # Check if industry and receivable value meet conditions
  if (industry[i] == industry_name && receivable[i] > receivable_value) {
    
    # If conditions are met, increment count
    count <- count + 1
  }
}

# Print result
cat(paste("Number of firms in", industry_name, "with receivable above", receivable_value, "is", count))



