rm(list=ls())
library(Hmisc)



hr_df<-read.csv("//shared/shape_tier3/bu_co/persist/rzgutie/Training/Accelebrate Materials/data/hr.csv")

head(hr_df,3)
tail(hr_df,3)
str(hr_df)
summary(hr_df)
describe(hr_df)

chick_df<-read.table("//shared/shape_tier3/bu_co/persist/rzgutie/Training/Accelebrate Materials/data/chickweight.txt",sep="|")
colnames(chick_df)<-c("weight","TIme","Chick","Diet")


hr_df$age[hr_df$age>100]<- NA
hr_df$agecat <-cut(hr_df$age,
                   right = FALSE,
                   breaks = c(0, 30, 40, 99),
                   labels = c("under 30", "30´s", "40 and over")                   )
table(hr_df$agecat)
plot(hr_df$agecat)
str(hr_df$agecat)
str(hr_df)
library(ggplot2)
plot(hr_df$agecat,col="light blue")

ID<-c("AA","BA","AA","O")
is.numeric(ID)
ID<-factor(ID)
ID
is.factor(ID)
as.numeric(ID)
levels(hr_df$agecat)<-c("less than 30","thirties","40 plus")

library(microbenchmark)
fa<-function(n){
  x<-1
  for(i in 2:n) x<-c(x,1)
}

fb <- function(n){
  x<-numeric(n)
  for(i in 1:n){
    x[i]<-1
  }
}
n=1000
bench<-microbenchmark(fa(n),fb(n))
bench

# Day 3 Manage data in R

library(dplyr)
set.seed(42)
sample_n(mtcars,4,replace=T)
sample_n(hr_df,30,replace=T)
sample_frac(hr_df,.1,replace=T)
sample_n(hr_df,30,replace=F)


hr_df%>%sample_frac(.2)%>%select(c(1:5))

# Merging datasets


df1 <- data.frame(Cust_ID = c(10:12),
                  Acc_ID = c(110:115),
                  AccType = c(rep("Checking", 3),
                              rep("Saving", 3)))
df2 <- data.frame(Customer_ID = c(10:11),
                  State = c("Oklahoma",
                            "Texas"))
df3 <- data.frame(Customer_ID = c(10:12),
                  Credit = c(rep("Bad", 1),
                             rep("Good", 2)))
# outer join

merge(x = df1, y = df2, by.x = "Cust_ID",
      by.y= "Customer_ID", all = FALSE)


#practice
person_df<-read.csv("//shared/shape_tier3/bu_co/persist/rzgutie/Training/Accelebrate Materials/data/ex_dm_person_demo.csv")
cluster_df<-read.csv("//shared/shape_tier3/bu_co/persist/rzgutie/Training/Accelebrate Materials/data/ex_dm_cluster_demo.csv")
library(Hmisc)
describe(person_df)
summary(person_df)
nrow(person_df)
str(person_df)
describe(person_df$Education)

as.character(person_df$Education)

person_df$Education <- tolower(person_df$Education)
as.factor(person_df$Education)
person_df$Education[person_df$Education == ""]<-NA
as.factor(person_df$Education)
str(person_df$p_id)
as.character(person_df$p_id)
as.Date(person_df$Baseline_date)
person_df[person_df=="999"]<-NA
describe(person_df$Baseline_date)
str(person_df$Baseline_date)
as.character(person_df$Baseline_date)

person_df$Baseline_date<-as.Date(person_df$Baseline_date, "%m-%d-%y")
str(person_df$date)
str(person_df)
library(DescTools)
person_df$lab_value_1[which(person_df$lab_value_1 %in% Outlier(person_df$lab_value_1))]<-NA
person_df$lab_value_2[which(person_df$lab_value_2 %in% Outlier(person_df$lab_value_2))]<-NA
person_df$lab_value_3[which(person_df$lab_value_3 %in% Outlier(person_df$lab_value_3))]<-NA
summary(person_df$lab_value_3)

person_df$Lab1_lab2<-person_df$lab_value_1/person_df$lab_value_2

mutate(person)
x <- 3
y <- ifelse(x %% 2 == 0, 1, 0)


str(person_df)
str(z)
person_df$lab_value_1[person_df$lab_value_1==z]<-NA



#### Day 3

summarize(mtcars,avg_mpg=mean(mpg))
str(summarize(mtcars,avg_mpg=mean(mpg)))
type(avg_mpg)

mtcars_grouped<-group_by(mtcars,cyl)
summarize(mtcars_grouped,avg_mpg=mean(mpg))
mutate(mtcars_grouped,avg_mpg=mean(mpg))


mtcars%>%group_by(cyl)%>%summarize(Numver_Obs=n(),
                                   avg_mpg=mean(mpg),
                                   median_mpg=median(mpg))

slice(mtcars_grouped,1)
sample_n(mtcars_grouped,2)


tbl1<-mtcars%>%group_by(gear,cyl)%>%summarize(sd_deviation=sd(mpg),
                                   avg_mpg=mean(mpg),
                                   median_mpg=median(mpg))
#reshaping data
library(reshape2)
library(dplyr)
dcast(tbl1,cyl-gear,value.var=`avg`)


#caterorical data berkeley
berkeley<-read.csv("//shared/shape_tier3/bu_co/persist/rzgutie/Training/Accelebrate Materials/data/berkleydf.csv")
berkeley%>%summarize(admit_rate=sum(Admit=="Admitted")/length(Admit))

berkeley %>% group_by(Gender) %>% summarize(admit_rate=sum(Admit=="Admitted")/length(Admit))
library(ggplot2)
berkeley %>% group_by(Gender,Dept) %>% summarize(admit_rate=sum(Admit=="Admitted")/length(Admit))%>%
ggplot(aes(x=Dept,y=admit_rate,fill=Gender)) +  geom_bar(position="dodge",stat="identity")+
  geom_point(size=10)

#appling functions to multidimensional object
#every column has the same type and the same length
# apply the parse of function parameters  do after a comma , not ()

apply(mtcars,2,mean)
head(iris)

sapply(mtcars,mean)


#control flow

#do a progresss bar for know dthe progress os a big loop


for(idx in c("mpg","hp")){
  cat("Mean", idx,"for the mtcars data is",
      mean(mtcars[,idx]), "\n")
}
pb<-txtProgressBar(min=0,max=n, char="|",style=3)
n<-20000
for(i in 1:n){
  setTxtProgressBar(pb,i)
}








