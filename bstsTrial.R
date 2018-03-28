setwd("F:\\GoogleDrive\\TestDataforSony")
library(prophet)
library(dplyr)
library(ggplot2)


filename <- "POS_test.csv"
mydata <- read.csv(filename, header = TRUE) 
md <- dplyr::tbl_df(mydata)

ds0 <- mydata %>% 
        group_by(as.numeric(Week_Of_Day) ) %>%
        summarize(sales = sum(Amount))

ds1 <- mydata %>% 
        group_by(Date) %>%
        summarize(sales = sum(Amount))

ds2 <- mydata %>% 
        group_by(Date, Item_ID) %>%
        summarize( sales = sum(Amount) )

ds2sep <- ds2 %>%
        group_by(Date)

ggplot(ds, aes( x=Week_Of_Day, y= sales))+
        geom_bar(aes(fill = group))+
        ggtitle("Sales by day")
       


Item_ID_List <- unique(md$Item_ID)
mdweekday1<- dplyr::filter(md, Week_Of_Day==1)
mdweekday2<- dplyr::filter(md, Week_Of_Day==2)
mdweekday3<- dplyr::filter(md, Week_Of_Day==3)
mdweekday4<- dplyr::filter(md, Week_Of_Day==4)
mdweekday5<- dplyr::filter(md, Week_Of_Day==5)
mdweekday6<- dplyr::filter(md, Week_Of_Day==6)
mdweekday7<- dplyr::filter(md, Week_Of_Day==7)

a1<- rle(sort(mdweekday1$Item_ID))
b1 <- data.frame(number=a1$values, n=a1$lengths)
plot(b1$number, b1$n)

a2<- rle(sort(mdweekday2$Item_ID))
b2 <- data.frame(number=a2$values, n=a2$lengths)
plot(b2$number, b2$n)


plot(mydata$Week_Of_Day  ~ sum(mydata$Amount))