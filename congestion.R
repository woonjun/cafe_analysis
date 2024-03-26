install.packages("dplyr")
install.packages("gplot2")
library(dplyr)
library(ggplot2)

setwd("c:\\r_temp")
getwd( )

data <- read.csv("서울시 상권분석서비스(추정매출-상권).csv", fileEncoding = "CP949")


filtered_data <- data %>%
  filter(서비스_업종_코드_명 == "커피-음료") %>%
  filter(월요일_매출_금액 > 0, 화요일_매출_금액 > 0, 수요일_매출_금액 > 0, 
         목요일_매출_금액 > 0, 금요일_매출_금액 > 0, 토요일_매출_금액 > 0, 일요일_매출_금액 > 0)

average_sales1 <- filtered_data %>%
  group_by(상권_구분_코드_명) %>%
  summarise(평균_월요일_매출 = mean(월요일_매출_금액),
            평균_화요일_매출 = mean(화요일_매출_금액),
            평균_수요일_매출 = mean(수요일_매출_금액),
            평균_목요일_매출 = mean(목요일_매출_금액),
            평균_금요일_매출 = mean(금요일_매출_금액),
            평균_토요일_매출 = mean(토요일_매출_금액),
            평균_일요일_매출 = mean(일요일_매출_금액),
            .groups = 'drop') # 그룹화된 데이터 구조를 유지하지 않고 결과를 반환합니다.

print(average_sales1, n = Inf, width = Inf)



p <- ggplot(data = average_sales1) +
  geom_col(aes(x = "월요일", y = 평균_월요일_매출), fill = 'blue') +
  geom_col(aes(x = "화요일", y = 평균_화요일_매출), fill = 'red') +
  geom_col(aes(x = "수요일", y = 평균_수요일_매출), fill = 'green') +
  geom_col(aes(x = "목요일", y = 평균_목요일_매출), fill = 'orange') +
  geom_col(aes(x = "금요일", y = 평균_금요일_매출), fill = 'purple') +
  geom_col(aes(x = "토요일", y = 평균_토요일_매출), fill = 'brown') +
  geom_col(aes(x = "일요일", y = 평균_일요일_매출), fill = 'pink') +
  labs(x = "요일", y = "평균 매출금액") +
  facet_wrap(~ 상권_구분_코드_명, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # x축 텍스트를 기울여서 표시합니다.

# 그래프를 출력합니다.
print(p)




filtered_data <- data %>%
  filter(서비스_업종_코드_명 == "커피-음료") %>%
  filter(시간대_00.06_매출_금액 > 0, 시간대_06.11_매출_금액 > 0, 시간대_11.14_매출_금액 > 0, 
         시간대_14.17_매출_금액 > 0, 시간대_17.21_매출_금액 > 0, 시간대_21.24_매출_금액 > 0)

average_sales2 <- filtered_data %>%
  group_by(상권_구분_코드_명) %>%
  summarise(평균_00.06_매출 = mean(시간대_00.06_매출_금액),
            평균_06.11_매출 = mean(시간대_06.11_매출_금액),
            평균_11.14_매출 = mean(시간대_11.14_매출_금액),
            평균_14.17_매출 = mean(시간대_14.17_매출_금액),
            평균_17.21_매출 = mean(시간대_17.21_매출_금액),
            평균_21.24_매출 = mean(시간대_21.24_매출_금액),
            .groups = 'drop') # 그룹화된 데이터 구조를 유지하지 않고 결과를 반환합니다.

print(average_sales2, n = Inf, width = Inf)



p <- ggplot(data = average_sales2) +
  geom_col(aes(x = "00.06", y = 평균_00.06_매출), fill = 'blue') +
  geom_col(aes(x = "06.11", y = 평균_06.11_매출), fill = 'red') +
  geom_col(aes(x = "11.14", y = 평균_11.14_매출), fill = 'green') +
  geom_col(aes(x = "14.17", y = 평균_14.17_매출), fill = 'orange') +
  geom_col(aes(x = "17.21", y = 평균_17.21_매출), fill = 'purple') +
  geom_col(aes(x = "21.24", y = 평균_21.24_매출), fill = 'brown') +
  labs(x = "시간", y = "평균 매출금액") +
  facet_wrap(~ 상권_구분_코드_명, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # x축 텍스트를 기울여서 표시합니다.

# 그래프를 출력합니다.
print(p)


developed_sales1 <- filter(average_sales1, 상권_구분_코드_명 == "발달상권")
developed_sales2 <- filter(average_sales2, 상권_구분_코드_명 == "발달상권")

# 요일별로 데이터를 선택합니다.
days <- c("월요일", "화요일", "수요일", "목요일", "금요일","토요일","일요일")

# 시간대별로 데이터를 선택합니다.
times <- c("00.06","06.11", "11.14", "14.17", "17.21", "21.24")

# 데이터를 재구성합니다.
merged_sales <- data.frame(상권_구분_코드_명 = developed_sales1$상권_구분_코드_명)

for (day in days) {
  for (time in times) {
    # 요일과 시간대에 따라 컬럼 이름을 생성합니다.
    sales1_colname <- paste("평균", day, "매출", sep = "_")
    sales2_colname <- paste("평균", time, "매출", sep = "_")
    
    # average_sales1과 average_sales2에서 해당 컬럼을 선택합니다.
    merged_sales[[paste(day, time, sep = "_")]] <- developed_sales1[[sales1_colname]] * developed_sales2[[sales2_colname]]
  }
}

merged_sales

all_values <- as.vector(unlist(merged_sales[-1]))
all_values
# min-max 정규화 함수 적용
normalized_values <- (all_values - min(all_values)) / (max(all_values) - min(all_values))

normalized_sales <- matrix(normalized_values, nrow = 1)
colnames(normalized_sales) <- colnames(merged_sales)[-1]

print(normalized_sales)

write.csv(normalized_sales, file = "min-max_sales.csv", row.names = FALSE)

