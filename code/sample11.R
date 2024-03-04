boxplot(temp2$cost, main = "학생 금액 요약")
A = c()
for(i in seq(0, 4200000, 200000)){
  k = 0
  print(i)
  for(j in 1:length(temp2$식별번호)){
    if(temp2$cost[j] >=i && temp2$cost[j] <i+200000){
      k = k+1
    }
  }
  A = c(A, k)
}
A
barplot(A~seq(0, 4200000, 200000), main = "금액 분포", col = rainbow(25), xlab = "가격", ylab = "n")