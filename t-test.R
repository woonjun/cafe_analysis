library(readxl)

setwd("c:\\r_temp")
getwd( )

bill <-read_excel("데이터생성.xlsx",sheet=1)
satisfy <-read_excel("데이터생성.xlsx",sheet=2)
vip <- read_excel("데이터생성.xlsx",sheet=3)
congestion<-read_excel("데이터생성.xlsx",sheet=4)
time<-read_excel("데이터생성.xlsx",sheet=5)
data <- read_excel("사용데이터.xlsx")


#사용 데이터와 영수증 데이터 결합합
data_selected <- data[, c("name...1", "name...2", "user_id", "plug", "start", "end", "bill")]
merged_data <- data.frame()
data_subset <- data_selected[1:51, ]
merged_data <- merge(data_subset, bill, by.x = "bill", by.y = "bill", all.x = TRUE)
merged_data

#병합데이터에, 만족도와 단골고객 데이터 결합합
merged_data <- merge(merged_data, satisfy, by.x = c("user_id", "name...1"), by.y = c("user_id", "카페명"), all.x = TRUE)
merged_data <- merge(merged_data, vip, by.x = c("user_id", "name...1"), by.y = c("user_id", "카페명"), all.x = TRUE)
merged_data <- merge(merged_data, time, by.x = c("user_id", "name...1"), by.y = c("user_id", "카페명"), all.x = TRUE)

# 시간대에 따라 새로운 열 생성
merged_data$weekday <- weekdays(as.Date(merged_data$start))
merged_data$hour <- as.integer(format(as.POSIXct(merged_data$start), "%H"))
merged_data$time_slot <- ifelse(merged_data$hour >= 0 & merged_data$hour < 6, "00~06",
                                ifelse(merged_data$hour >= 6 & merged_data$hour < 11, "6~11",
                                       ifelse(merged_data$hour >= 11 & merged_data$hour < 14, "11~14",
                                              ifelse(merged_data$hour >= 14 & merged_data$hour < 17, "14~17",
                                                     ifelse(merged_data$hour >= 17 & merged_data$hour < 21, "17~21", "21~00")))))

# merged_data에 혼잡도 데이터 추가
merged_data <- merge(merged_data, congestion, by.x = c("weekday", "time_slot"), by.y = c("요일", "시간대"), all.x = TRUE)

#plug*(end-start)로 전기 사용량 계산산
time_diff_hours <- difftime(merged_data$end, merged_data$start, units = "hours")
merged_data$전기사용량 <- merged_data$plug * as.numeric(time_diff_hours)

#실제로 t-test에서 사용하는 column만 남기기
merged_data_selected <- merged_data %>%
  select(name...1, user_id, bill, 이용금액, 만족여부, 방문회수, 혼잡도, 전기사용량,이용시간)


install.packages("openxlsx")
library(openxlsx)
write.xlsx(merged_data_selected, file = "merged_data.xlsx", rowNames = FALSE)

#이용금엑애 따른 t-test -> pvalue : 2.481e-05
high_spend_group = merged_data$만족여부[merged_data$이용금액 > 10000]
low_spend_group = merged_data$만족여부[merged_data$이용금액 <= 10000]
t_test_result = t.test(high_spend_group, low_spend_group, na.rm = TRUE)
t_test_result

#혼잡도에 따른 t-test -> pvaue : 0.02522
high_group = merged_data$만족여부[merged_data$혼잡도 > 4]
low_group = merged_data$만족여부[merged_data$혼잡도 <= 4]
t_test_result = t.test(high_group, low_group, na.rm = TRUE)
t_test_result

#단골에 따른 t-test -> p-value = 0.2701
high_group = merged_data$만족여부[merged_data$방문회수 > 15]
low_group = merged_data$만족여부[merged_data$방문회수 <= 15]
t_test_result = t.test(high_group, low_group, na.rm = TRUE)
t_test_result

#전기사용량량에 따른 t-test -> p-value = 0.3864
high_group = merged_data$만족여부[merged_data$전기사용량 > 8]
low_group = merged_data$만족여부[merged_data$전기사용량 <= 8]
t_test_result = t.test(high_group, low_group, na.rm = TRUE)
t_test_result

#회귀분석 진행행
model <- lm(만족여부 ~ 전기사용량 + 방문회수 + 혼잡도 + 이용금액, data=merged_data)
summary(model)

model <- lm(만족여부 ~혼잡도 + 이용금액, data=merged_data)
summary(model)

#로지스틱 회귀분석 진행
merged_data$binary_satisfaction <- ifelse(merged_data$만족여부 >= 4, 1, 0)

logistic_model <- glm(binary_satisfaction ~ 혼잡도 + 이용금액, 
                      family = binomial(link = "logit"), 
                      data = merged_data)

summary(logistic_model)

################### 여기서부터는 그래프 그리기 용

breaks <- c(-Inf, 7000, 14000, 21000, Inf)
labels <- c("7000원 이하", "7000~14000원", "14000~21000원", "21000원 이상")
bill$금액범주 <- cut(bill$이용금액, breaks = breaks, labels = labels, right = FALSE)
freq <- table(bill$금액범주)
ggplot(data = as.data.frame(freq), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("이용금액 범주") +
  ylab("빈도수") +
  ggtitle("이용금액 별 빈도수")

# 방문회수 최대값을 확인하고 적절한 구간을 설정
breaks <- seq(0, 30, by = 5)
labels <- paste(head(breaks, -1), "~", tail(breaks, -1) - 1)
time$방문회수범주 <- cut(vip$방문회수, breaks = breaks, labels = labels, include.lowest = TRUE)
freq <- table(time$방문회수범주)
ggplot(data = as.data.frame(freq), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("방문회수 범주") +
  ylab("빈도수") +
  ggtitle("방문회수 범주별 빈도수") +

freq <- table(satisfy$만족여부)
ggplot(data = as.data.frame(freq), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("만족회수") +
  ylab("빈도수") +
  ggtitle("만족회수별 빈도수")

freq <- table(time$이용시간)
ggplot(data = as.data.frame(freq), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  xlab("이용시간") +
  ylab("빈도수") +
  ggtitle("이용시간별 빈도수")

