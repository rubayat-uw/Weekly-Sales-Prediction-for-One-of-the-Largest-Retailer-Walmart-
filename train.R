prjTrain<-function(train){
  
  stores$Store <- factor(stores$Store)
  train$Store <- factor(train$Store)
  train <- full_join(train,stores,by=c("Store"))
  train$WeekNum <- format(as.Date(train$Date), "%V")
  train$Returns <- lapply(train$Weekly_Sales,function(sales){
    ifelse(sales < 0,sales,0)})
  train$Weekly_Sales <- lapply(train$Weekly_Sales,function(sales){
    ifelse(sales > 0,sales,0)
  })
  final_data <- data.frame(Store=factor(),Date=as.Date(character()),Weekly_Sales=numeric(),IsHoliday=logical(),Type=factor(),WeekNum=factor())
  aggregate_sales <- function(){
    for(i in 1:45){
      store_data <- train %>% filter(Store == i)
      dates <- unique(train$Date)
      for(next_date in seq_along(dates)){
        current_date <- unique(train$Date)[[next_date]]
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
  train <- left_join(final_data1,features,by=c("Store","Date","IsHoliday"))
  # Make the NA markdown as 0
  train$MarkDown1 <- sapply(train$MarkDown1, function(value){
    ifelse(is.na(value),0,value)
  })
  train$MarkDown2 <- sapply(train$MarkDown2, function(value){
    ifelse(is.na(value),0,value)
  })
  train$MarkDown3 <- sapply(train$MarkDown3, function(value){
    ifelse(is.na(value),0,value)
  })
  train$MarkDown4 <- sapply(train$MarkDown4, function(value){
    ifelse(is.na(value),0,value)
  })
  train$MarkDown5 <- sapply(train$MarkDown5, function(value){
    ifelse(is.na(value),0,value)
  })
  
  return(train)
}

