prjTest<-function(test){
  stores$Store <- factor(stores$Store)
  test$Store <- factor(test$Store)
  test <- full_join(test,stores,by=c("Store"))
  test$WeekNum <- format(as.Date(test$Date), "%V")
  test$Returns <- lapply(test$Weekly_Sales,function(sales){
    ifelse(sales < 0,sales,0)})
  test$Weekly_Sales <- lapply(test$Weekly_Sales,function(sales){
    ifelse(sales > 0,sales,0)
  })
  final_data <- data.frame(Store=factor(),Date=as.Date(character()),Weekly_Sales=numeric(),IsHoliday=logical(),Type=factor(),WeekNum=factor())
  aggregate_sales <- function(){
    for(i in 1:45){
      store_data <- test %>% filter(Store == i)
      dates <- unique(test$Date)
      for(next_date in seq_along(dates)){
        current_date <- unique(test$Date)[[next_date]]
        date_data <- store_data %>% filter(Date==current_date)
      #Add all the weekly sales
        net_sales <- sum(unlist(date_data$Weekly_Sales)) - sum(unlist(date_data$Returns))
      #Construct the data frame and append it
        next_row <- data.frame(Store=i,Date=current_date,Weekly_Sales=net_sales,IsHoliday=date_data$IsHoliday[[1]],Type=date_data$Type[[1]],WeekNum=date_data$WeekNum)
        next_row$Store <- factor(next_row$Store)
        final_data <- rbind(final_data,next_row)
      }
    }
    return(final_data)
  }
# Sum the sales by store without taking into account each department
  final_data <- aggregate_sales()
  final_data1 <-final_data[!duplicated(final_data), ]
  features$Store <- factor(features$Store)
#Merge our final_data with our features
  test <- left_join(final_data1,features,by=c("Store","Date","IsHoliday"))
# Make the NA markdown as 0
  test$MarkDown1 <- sapply(test$MarkDown1, function(value){
    ifelse(is.na(value),0,value)
  })
  test$MarkDown2 <- sapply(test$MarkDown2, function(value){
    ifelse(is.na(value),0,value)
  })
  test$MarkDown3 <- sapply(test$MarkDown3, function(value){
    ifelse(is.na(value),0,value)
  })
  test$MarkDown4 <- sapply(test$MarkDown4, function(value){
    ifelse(is.na(value),0,value)
  })
  test$MarkDown5 <- sapply(test$MarkDown5, function(value){
    ifelse(is.na(value),0,value)
  })

  return(test)
}

