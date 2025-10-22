price <- read.csv("/Users/jamesli/Downloads/Housing Price in Beijing.csv", sep=',', stringsAsFactors=F, fileEncoding="latin1")
price$floor <- as.numeric(sapply(price$floor, function(x) strsplit(x,' ')[[1]][2]))
str(price)
library(dplyr)
library(psych)
price2 <- select(price, id, tradeTime, totalPrice, price, square, livingRoom, drawingRoom, 
                    kitchen, bathRoom, floor, communityAverage, elevator, subway, buildingType, buildingStructure)
str(price2)
head(price2)

price2 <- price2 %>% 
  mutate(buildingType = case_when(buildingType == 1 ~ "Tower",
                                  buildingType == 2 ~ "Bungalow",
                                  buildingType == 3 ~ "Plate/Tower",
                                  buildingType == 4 ~ "Plate"))
price2 <- price2 %>% 
  mutate(buildingStructure = case_when(buildingStructure == 1 ~ "Unavailable",
                                       buildingStructure == 2 ~ "Mixed",
                                       buildingStructure == 3 ~ "Brick/Wood",
                                       buildingStructure == 4 ~ "Brick/Concrete",
                                       buildingStructure == 5 ~ "Steel",
                                       buildingStructure == 6 ~ "Steel/Concrete"))
price2 <- price2 %>% 
  mutate(elevator = case_when(elevator == 1 ~ "Has_Elevator",
                              elevator != 1 ~ "No_elevator"))
price2 <- price2 %>% 
  mutate(subway = case_when(subway == 1 ~ "Has_Subway",
                            subway != 1 ~ "No_Subway"))

str(price2)
head(price2)

# Find total NA values in price2
sum(is.na(price2))
# Find total NA values by column
sapply(price2, function(price2) sum(is.na(price2)))
price2 <- na.omit(price2)
new_price <- price2[,3:11]
str(new_price)

# Change Character to numerical type
new_price$livingRoom <- as.numeric(new_price$livingRoom)
new_price$bathRoom <- as.numeric(new_price$bathRoom)
new_price$drawingRoom <- as.numeric(new_price$drawingRoom)

# Check type 
str(new_price)
head(new_price)

summary(new_price)
describe(new_price)

library(ggplot2)

# Histogram of price
ggplot(new_price, aes(price)) + geom_histogram(fill = "steelblue") + 
  labs(title = "Histogram of price")
ggplot(new_price, aes(totalPrice)) + geom_histogram(fill = "steelblue") +
  labs(title = "Histogram of totalPrice", x =" totalPrice(K)") + xlim(0, 2000)

# Corrplots
library(corrplot, quietly = TRUE)
cor(new_price)
COR1 <- cor(new_price, method = "pearson")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(COR1, method = "color", col = col(200), type = "upper", order = "hclust", 
         addCoef.col = "black", tl.col = "black", tl.srt = 30, 
         sig.level = 0.01, insig = "blank")

# Boxplots
ggplot(price2 , aes(x = buildingType, y = price, color = buildingType)) + geom_boxplot() + 
  labs(title = "Prices vs Building Type", y = " Price/Sqft")
ggplot(price2 , aes(x = buildingStructure, y = price, color = buildingStructure)) + geom_boxplot() + 
  labs(title = "Prices vs Building Structure", y = " Price/Sqft")
ggplot(price2, aes(x = elevator, y = price, color = elevator)) + geom_boxplot() + 
  labs(title = "Prices vs elevator", y =" Price/Sqft")
ggplot(price2, aes(x = subway, y = price, color = subway)) + geom_boxplot() + 
  labs(title = "Prices vs subway", y =" Price/Sqft")

# Scatterplots
lm(price2$price ~ price2$totalPrice)

ggplot(price2, aes(x = price, y = totalPrice)) +
  geom_point(aes(color = buildingType), size = 0.1) +
  geom_smooth(method = lm) + ggtitle("By buildingType")

ggplot(price2, aes(x = price, y = totalPrice)) +
  geom_point(aes(color = buildingStructure), size = 0.1) +
  geom_smooth(method = lm) + ggtitle("By buildingStructure")

ggplot(price2, aes(x = price, y = totalPrice, color = buildingType)) + 
  geom_point(size = 0.1) + geom_smooth(aes(group = buildingType), method = lm) + 
  ggtitle("By buildingType") 

ggplot(price2, aes(x = price, y = totalPrice, color = buildingStructure)) + 
  geom_point(size = 0.1) + geom_smooth(aes(group = buildingStructure), method = lm) + 
  ggtitle("By buildingStructure") 

# Separate tradeTime into "Month", "Day", "Year", and subset four features
library(tidyr)
library(knitr)
library(dygraphs)
library(lubridate)
library(xts)
house_price_year <- separate(price2, tradeTime, c("Year", "Month", "Day"), sep = "-")
house_price_year <- subset(house_price_year, select = c("Year", "Month", "totalPrice", "price"))
head(house_price_year)
house_price_year$monthlyTrade <- as.Date(paste0(house_price_year$Year, '-', house_price_year$Month,'-01'))
head(house_price_year)

kable(house_price_year %>% group_by(house_price_year$monthlyTrade) %>% summarize(n = n()))
house_price_year2 <- house_price_year %>% filter(monthlyTrade >= ymd("2010-01-01") & monthlyTrade <= ymd("2018-01-01")) %>% 
  group_by(monthlyTrade) %>%  
  summarize(mean = mean(price))
house_price_year2
data_xts <- xts(house_price_year2[,-1], order.by = house_price_year2$monthlyTrade)
dygraph(data_xts, main = "Average Housing Price by Month", 
        ylab = "Average Housing Price") %>%
  dySeries("mean", label = "Mean Price/SQF") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)

# Question 1
set.seed(2)
t.test(sample(price2$price, 400), mu = mean(price2$price))
# The critical value of t for a two-tailed test with df = 19 and α = .05
qt(p = .05 / 2, df = 399, lower.tail = FALSE)

# Question 2
tower_price <- price2 %>% filter(buildingType == "Tower") %>% select(price)
bungalow_price <- price2 %>% filter(buildingType == "Bungalow") %>% select(price)
t.test(tower_price, bungalow_price, paired = FALSE, alternative = "two.sided", var.equal = FALSE)

# Question 3
price_2016 <- house_price_year %>% filter(Year == "2016") %>% select(price)
price_2017 <- house_price_year %>% filter(Year == "2017") %>% select(price)
t.test(price_2016, price_2017, paired = FALSE, alternative = "less", var.equal = FALSE)
