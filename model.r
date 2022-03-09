library(dplyr)
library(readxl)
library(arules)
library(gtools)
file <- read.csv("grc.csv")
data <- as.data.frame(file)
custs = strsplit(as.vector(data$customer),' ')
prods = strsplit(as.vector(data$items),',')
prods
cid <- unique(unlist(custs))
items <- unique(unlist(prods))
totals <- c()
rnds <- 1:length(cid)

for (i in rnds){
  totals[i] <- sum(filter(data, rnd == i)$total)
}
Ages <- c()

for(i in rnds){
  Ages[i] <- unique(filter(data, rnd == i)$age)
}

names <- c()

for(i in rnds){
  names[i] <- unique(filter(data, rnd == i)$customer)
}

cities <- c()

for(i in rnds){
  cities[i] <- unique(filter(data, rnd == i)$city)
}

Cashes <- c()

for(i in rnds){
  Cashes[i] <- sum(filter(data, rnd == i, paymentType == "Cash")$total)
}

Credits <- c()

for(i in rnds){
  Credits[i] <- sum(filter(data, rnd == i, paymentType == "Credit")$total)
}

data_set <- data.frame(
  customer = names,
  age = Ages,
  city = cities,
  total = totals,
  payments = c(Cashes + Credits)
)

totalCities <- c()
Cities <- unique(cities) 

for(i in 1:length(Cities)){
  totalCities[i] <- sum((filter(data_set, city == Cities[i])$total)/1000) 
} 

sCities <- data.frame(
  city = Cities
  ,cTotal = totalCities
)

par(mfrow=c(2,2),
    bg="gray8",
    fg="dodgerblue4"
)

sumTable <- data.frame(
  payment = c("Total Cash","Total Credit"),
  total_payment = c(sum(Cashes), sum(Credits))
)

pie( #we realize that cash and credit are equivalent 
  x= sumTable$total_payment,
  labels = sumTable$payment,
  main = "Comparing cash and credit totals",
  col = c("dodgerblue4","gray10"),
  border = "gray8",
  radius = 1.1,
  init.angle = 30,
  col.main = "dodgerblue4",
  cex.main=1.4
)


spendingInEachAge <- group_by(data_set,age)
spendingInEachAge <- summarise(spendingInEachAge, totalSpending=sum((total/1000)))

plot(
  main = "Comparing each age and sum of total spending",
  y = spendingInEachAge$totalSpending,
  x = spendingInEachAge$age,
  xlab = "Age",
  ylab = "Total Spending in K",
  col = c("dodgerblue4"),
  pch = 19,
  col.main = "dodgerblue4",
  col.lab = "dodgerblue4",
  col.axis = "dodgerblue4",
  las=1,
  cex.main=1.4,
  cex.lab=1.3
)

spendingArranged <- arrange(sCities, desc(cTotal), city)
spendingTable <- data.frame(
  city = spendingArranged$city,
  total = spendingArranged$cTotal)


barplot(
  height = spendingTable$total,
  names = spendingTable$city,
  ylab = "Total Spending in K",
  main="Spending in each city",
  border = "dodgerblue4",
  xlab = "City",
  col = "dodgerblue4",
  las=1,
  cex.names=1.1,
  col.main = "dodgerblue4",
  col.lab  = "dodgerblue4",
  col.axis = "dodgerblue4",
  cex.main=1.4,
  cex.lab=1.1
)

boxplot(
  x = (data_set$total/1000),
  main = "Distribution of total spending",
  xlab = "Total Spending in K",
  col= "dodgerblue4",
  border = "gray21",
  col.main    = "dodgerblue4",
  col.lab = "dodgerblue4",
  col.axis = "dodgerblue4",
  las=1,
  cex.lab=1.3,
  cex.main=1.4
)

points <- cbind(c(data_set$total),c(data_set$age))

colnames(points)<- c("Total Spending","Age" )

valid <- F
while(valid==F){
  number_of_cluster <- as.numeric(readline("Enter The Number of Clusters between 2 and 4: "))
  if(number_of_cluster < 2){
    print("Number of cluster can't be less than 2")
  }else if(number_of_cluster > 4){
    print("Number of cluster can't be more than 4")
  } else{
    valid<-T
  }
}

result <- kmeans(points, centers = number_of_cluster)
DB <- cbind(data_set,group = result$cluster)
print(DB)


valid <- F
while(valid==F){
  min_support <-as.numeric(readline(prompt = "Enter the minimum support (note:the minimum support must be between[1 and 100]%): "))
  if(min_support < 1){
    print("Minimum Support can't be less than 1%")
  }else if(min_support>100){
    print("Minimum Support can't be more than 100%")
  } else{
    valid<-T
  }
}


valid<-F
while(valid==F){
  min_conf <-as.numeric(readline(prompt = "Enter the minimum confidence (note:the minimum support must be between[1 and 100]%): "))
  if(min_conf<1){
    print("Minimum Confidence can't be less than 1%")
  } else if(min_conf>100){
    print("Minimum Confidence can't be more than 100%")
  } else{
    valid<-T
  }
}

data_prods <- as(prods, "transactions")
apriori_rules<-apriori(data_prods,parameter = list(supp = min_support/100 , conf = min_conf/100 ,minlen=2))
inspect(apriori_rules)
