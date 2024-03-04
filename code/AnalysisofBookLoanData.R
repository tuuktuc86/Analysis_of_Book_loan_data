# install.packages('readxl')
# install.packages('openxlsx')
# install.packages('tidyverse')
library(tidyverse)
library(readxl)
library(openxlsx)
library(dplyr)
book_data = read_xls("C:/Users/User/Desktop/dataset/book_rental.xls")
#데이터 불러오기
Sys.setlocale("LC_TIME", "English_United States")
book_data$식별번호 = gsub("X_", "", book_data$식별번호)
book_data$식별번호 = as.integer(book_data$식별번호)
#식별번호 정수화

#mode(book_data$식별번호)
book_data = arrange(book_data, 식별번호)
#정렬
book_data



# 책 대출 인원 파악

temp2 = book_data %>% count(식별번호)
temp2

#많이 빌린 책
temp3 = book_data %>% count(도서명)
temp3

book_data$대출년도 = substr(book_data$대출일,1,4) %>% as.integer(book_data$대출일)
book_data$대출년도


book_data$대출월 = substr(book_data$대출일,6,7) %>% as.integer(book_data$대출일)

book_data$대출시간 = substr(book_data$대출일,11,12) %>% as.integer(book_data$대출일)
book_data$대출일자 = substr(book_data$대출일,9,10) %>% as.integer(book_data$대출일)
book_data$대출요일 = weekdays(as.Date(paste(book_data$대출년도, book_data$대출월, book_data$대출일자, sep="-")))
book_data$마일리지 = grepl("마일리지", book_data$도서명)
temp3$마일리지 = grepl("마일리지", temp3$도서명)
temp5 = book_data %>% count(대출월)
temp5
for(i in 1:length(temp5$대출월)){    
  #날짜 시간 분리
  if(temp5$대출월[i] == 1 ||  temp5$대출월[i] == 2){
    temp5$n[i] = temp5$n[i]
  }else if(temp5$대출월[i] >= 3 && temp5$대출월[i] <= 6){
    temp5$n[i] = temp5$n[i] / 3
  }else if(temp5$대출월[i] >= 7 &&  temp5$대출월[i] <= 12){
    temp5$n[i] = temp5$n[i] / 2
  }
}
temp5_plot = barplot(n~대출월,temp5, main = "월별 대출 권수", col = rainbow(12))
text(x=temp5_plot,y=temp5$n,labels=temp5$n,pos=3,col="black")
temp5
#연도별 카운트
temp4 = book_data %>% count(대출년도)
temp4
temp4_plot = barplot(n~대출년도,temp4, main = "연도별 대출 권수", col = rainbow(3))
text(x=temp4_plot,y=temp4$n,labels=temp4$n,pos=3,col="black")

temp6 = book_data %>% count(대출시간)
temp6
temp6_plot = barplot(n~대출시간,temp6, main = "시간대별 대출 권수", col = rainbow(7))
text(x=temp6_plot,y=temp6$n,labels=temp6$n,pos=3,col="black")

temp7 = book_data %>% count(대출요일)
temp7$대출요일 = ordered(temp7$대출요일, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                         "Friday", "Saturday", "Sunday"))
temp7
temp7_plot = barplot(n~대출요일,temp7, main = "요일별 대출 권수", col = rainbow(7))
text(x=temp7_plot,y=c(1781, 1957, 1442, 1472, 1764, 3, 143),labels=c(1781, 1957, 1442, 1472, 1764, 3, 143),pos=3,col="black")
x = rep(0, times = length(temp2$식별번호))
x
temp2 = cbind(temp2, 마일리지점수 = x)
for(i in 1:length(book_data$도서명)){
  if(book_data$마일리지[i] == TRUE){
    temp2$마일리지점수[book_data$식별번호[i]] = temp2$마일리지점수[book_data$식별번호[i]]+1
  }
  
}
barplot(마일리지점수~식별번호,temp2)
#temp8 =  book_data %>% group_by(식별번호) %>% summarise(count = n(), 마일리지합계 = length(book_data$마일리지[book_data$마일리지== TRUE]))
#월별 도서량
#시간대별 분석 인원 분류
#저자를 뽑아내어 강연?
#마일리지 계산
#카테고리 뽑은 뒤 word cloud
#후기같은거 뽑아와서 이 책 검색하면 후기를 알려줄까?
#많이 빌린 책 추천 도서리스트

boxplot(temp2$n, main = "도서 대출 건수 box plot")
boxplot(temp2$마일리지점수, main = "도서 마일리지 점수 boxplot")

pie_1 = c(nrow(book_data[book_data$마일리지==TRUE,]), nrow(book_data[book_data$마일리지==FALSE,]))
pct = round(pie_1 / sum(pie_1) * 100, 1)
lab = paste(pct, "%")
pie(pie_1, init.angle = 90, col = rainbow(length(pie_1)), label = lab, main = "마일리지 대출 비율")
legend("topright", , c("마일리지 도서", "일반도서"), cex = 0.8, fill = rainbow(length(pie_1)))

pie_2 = c(nrow(temp3[temp3$마일리지==TRUE,]), nrow(temp3[temp3$마일리지==FALSE,]))
pct = round(pie_2 / sum(pie_2) * 100, 1)
lab = paste(pct, "%")
pie(pie_2, init.angle = 90, col = rainbow(length(pie_2)), label = lab, main = "마일리지 도서 비율")
legend("topright", , c("마일리지 도서", "일반도서"), cex = 0.8, fill = rainbow(length(pie_1)))

var(temp2$마일리지점수)
var(temp2$n)
var(temp2$cost)