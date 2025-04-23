library(xlsx)
library(tidyverse)
library(xgboost)
library(fastDummies)


# Functions 
remove_lag_lead <- function(data, target_col, n) {
  # Construct patterns for the target's lag/lead columns
  lag_cols <- paste0(target_col, "_lag", 1:n)
  lead_cols <- paste0(target_col, "_lead", 1:n)
  drop_cols <- c(lag_cols, lead_cols)
  
  # Drop only those columns that match and exist in the data
  cleaned_data <- data[, !(names(data) %in% drop_cols), drop = FALSE]
  
  return(cleaned_data)
}
remove_dummy <- function(data, target_col) {
  # Construct patterns for the target's lag/lead columns
  col1 <- paste0(target_col, "_2")
  col2 <- paste0(target_col, "_0")
  col3 <- paste0(target_col, "_1")
  drop_cols <- c(col1,col2,col3)
  
  # Drop only those columns that match and exist in the data
  cleaned_data <- data[, !(names(data) %in% drop_cols), drop = FALSE]
  
  return(cleaned_data)
}
# ---- IMPUTE: Replace NAs with class 2 (all data is discrete) ----
impute_class_3 <- function(data) {
  data[] <- lapply(data, function(col) {
    col[is.na(col)] <- 3
    return(col)
  })
  return(data)
}
# Identify Competitors
id_comps<- function(data){
  x<-colnames(data)
  comps=c()
  for (i in 1:length(x)){
    y <- paste0(x[i])
    a=stringi::stri_locate_all(pattern="_lag",y,fixed = T)
    b=stringi::stri_locate_all(pattern="_lead",y,fixed = T)
    if (2==length(a[[1]])){
      comps=c(comps,i)
    }
    else if (2==length(b[[1]])){
      comps=c(comps,i)
    }}
  return(comps)
}
# Remove No Change Columns
remove_no_change<- function(data,z){
  x<-colnames(data)
  col=c()
  if (z == 1) {
    for (i in 1:length(x)){
      y <- paste0(x[i])
      a=stringi::stri_locate_all(pattern="_1",y,fixed = T)
      if (2==length(a[[1]])){
        col=c(col,i)
      }
      b=stringi::stri_locate_all(pattern="_0",y,fixed = T)
      if (2==length(b[[1]])){
        col=c(col,i)
      }
    }
  } else if (z == -1){
    for (i in 1:length(x)){
      y <- paste0(x[i])
      a=stringi::stri_locate_all(pattern="_1",y,fixed = T)
      if (2==length(a[[1]])){
        col=c(col,i)
      }
      b=stringi::stri_locate_all(pattern="_2",y,fixed = T)
      if (2==length(b[[1]])){
        col=c(col,i)
      }
    }
  } else if (z == 0){
    for (i in 1:length(x)){
      y <- paste0(x[i])
      a=stringi::stri_locate_all(pattern="_1",y,fixed = T)
      if (2==length(a[[1]])){
        col=c(col,i)
      }
    }
  }
  return(col)
}


# Return top 20 XGB importance
top_20_function <- function (data, target_col, direction)  {
  
  data<-data[,-c(1,which(colnames(data) %in% c("Calendar.Date","SKU","Brand")))]
  
  data <- data + 1 
  
  #Denote Target & Window
  n <- 4  # Set the window of days
  
  
  #remove other competitors' same day events
  competitors<-colnames(data[,-id_comps(data)])
  non_comps<-id_comps(data)
  data<-data[,c(which(colnames(data) %in% target_col),non_comps)]
  
  
  #Remove Target Lag & Lead
  data <- remove_lag_lead(data,target_col,n)
  
  # character the Data 
  data <- as.data.frame(lapply(data, as.character))
  
  
  # Create Dummy Variables (ANY CHARACTER VALUES WILL BE CONVERTED TO DUMMY VARIABLES)
  data_dum<-dummy_columns(data,remove_selected_columns = T,ignore_na=T)
  
  # Remove Dummy Vars for target
  data_dum<-remove_dummy(data_dum,target_col)
  
  # Remove No Change Variables
  no_change <- remove_no_change(data_dum,direction)
  data_dum<-data_dum[,-no_change]
  
  # cbind taget col to dataset
  data<-cbind(data[,which(colnames(data) %in% target_col)],data_dum)
  colnames(data)[1]<-target_col
  
  # Remove Target NA Rows
  data<-data%>%
    filter(!is.na(data[,1]))
  
  #Remove Unwanted Levels
  if (direction == 1){
    data<-data%>%
      filter(data[,1]!=0)
  } else if (direction == -1){
    data<-data%>%
      filter(data[,1]!=2)
  }
  
  index<-sample(1:nrow(data),(2/3)*nrow(data))
  data.train<-data[index,]
  data.test<-data[-index,]
  
  
  # Data Prep
  # Separate features and labels
  train_data <- data.train[, -which(names(data.train) == target_col)]
  test_data  <- data.test[, -which(names(data.test) == target_col)]
  
  train_label <- data.train[[target_col]]
  test_label  <- data.test[[target_col]]
  
  if ((length(unique(test_label))>1)&(length(unique(train_label))>1)){
    
    
    
    # Ensure all features are numeric
    train_data_cleaned <- data.frame(lapply(train_data, as.numeric))
    test_data_cleaned  <- data.frame(lapply(test_data, as.numeric))
    
    if (direction == 1){
      train_label <- as.numeric(train_label)-direction
      test_label <- as.numeric(test_label)-direction
    } else {
      train_label <- as.numeric(train_label)
      test_label <- as.numeric(test_label)
    }
    
    # DMatrix for XGBoost
    dtrain <- xgb.DMatrix(data = as.matrix(train_data_cleaned), label = train_label)
    dtest  <- xgb.DMatrix(data = as.matrix(test_data_cleaned),  label = test_label)
    
    
    # Training Model
    num_class <- length(unique(train_label))
    watchlist <- list(eval = dtest, train = dtrain)
    
    if (direction %in% c(-1,1)){
      params <- list(
        max_depth = 2,
        eta = 0.3,
        subsample = 0.8,
        min_child_weight = 10,
        objective = "binary:logistic"
      )
      bst <- xgboost(
        data = dtest,
        params = params,
        nrounds = 2000,
        verbose = 0
      )
      pred <- predict(bst, dtest,type = "prob")
      for (j in 1:length(pred)){
        if(pred[j]<0.5){
          pred[j]=0
        }
        else{
          pred[j]=1
        }
      }
    } else {
      params <- list(
        max_depth = 2,
        eta = 0.3,
        subsample = 0.8,
        min_child_weight = 10,
        objective = "multi:softmax",
        num_class = num_class
      )
      bst <- xgboost(
        data = dtest,
        params = params,
        nrounds = 2000,
        verbose = 0
      )
      pred = predict(bst, dtest)
    }
    
    
    # Predict Using Model
    
    # Conf. Matrix
    conf_matrix <- table(Predicted = pred, Actual = test_label)
    print(conf_matrix)
    
    # Assess Model Accuracy
    accuracy <- sum(pred == test_label) / length(test_label)
    cat("Accuracy:", round(accuracy * 100, 2), "%\n")
    
    # Find and plot important features
    importance <- xgb.importance(model = bst)
    #xgb.plot.importance(importance[1:20])
    top_20<-importance
    return(top_20)
  } else {
    top_20 <- data.frame(Feature = character(),
                             Gain = numeric(),
                             Cover = numeric(),
                             Frequency = numeric())
    return(top_20)
  }
}

### ----- ^^ RUN EVERYTHING ABOVE BEFORE CONTINUING ^^ ----- ###




# INDIVIDUAL COMPANY ANALYSIS BY INDIVIDUAL CATEGORY
# Data
dataset = Home.Theater.Accessories_Events
noncomps = id_comps(dataset)

# Top Scraped Companies Info for Datset
top_companies_scraped<-names(head(sort(colSums(is.na(dataset[,-c(noncomps,1,which(colnames(dataset) %in% c("Calendar.Date","SKU","Brand")))]))),25))
all_companies_scraped<-names(dataset[,-c(noncomps,1,which(colnames(dataset) %in% c("Calendar.Date","SKU","Brand")))])

# Target Column
target_col = "B.H.Photo"
# Set Direction ( 0 = Bidirectional, 1 = Increasing Events Only, -1 = Decreasing Events Only)
direction = 0
# Execute Function to Return top 20
Top20 = top_20_function(dataset,target_col,direction)
Top20_Corr=Top20$Feature[1:20]
# View Top 20 List
Top20_Corr




# INDIVIDUAL COMPANY ANALYSIS BY ALL CATEGORIES ( estimated 5 - 10 Mins)
# Target Column
target_col = "Newegg"
# Set Direction ( 0 = Bidirectional, 1 = Increasing Events Only, -1 = Decreasing Events Only)
direction = 0

# List all DFs for Categories
categories = c("Computing.Accessories_Events","Floorcare_Events")#,"Headphones_Events","Home.Theater.Accessories_Events","Input.Devices_Events","iPad_Events","laptop.accessories_Events","MacBook_Events","Mounts_Events","Printers_Events","tablet.accessories_Events","Traffic.Appliances_Events")

# Create Main_Df to Output Gains for each Category
Main_Df <- data.frame(Company = character())

for (category in 1:length(categories)){
  dataset = get(categories[category])
  # Execute Function
  Top20_Corr = top_20_function(dataset,target_col,direction)
  
  # Display top 20 Correlated Companies (Lag/Lead)
  Companies <- substr(Top20_Corr$Feature, 1, nchar(Top20_Corr$Feature) - 3)
  Top_Df<-data.frame(Company = Companies, Gain = Top20_Corr$Gain)
  Top_Df <- Top_Df %>%
    group_by(Company) %>%
    summarise(Gain = sum(Gain), .groups = "drop") %>%
    arrange(desc(Gain))
  colnames(Top_Df)[2]=paste0("Gain - ",categories[category])
  Main_Df<-full_join(Top_Df,Main_Df,by = "Company")
}
# Name & View new DF
new_name <- paste0(gsub("[^A-Za-z0-9]", "_", target_col), "_df")
assign(new_name, Main_Df)
rm(Main_Df)
View(get(new_name))





# MASS Company Analysis (estimated time to run = 45 mins)
# List of target columns you want to analyze
target_cols <- c("Newegg","Newegg.MP","Target","Target.Marketplace","Home.Depot","Office.Depot")

# Set Direction ( 0 = Bidirectional, 1 = Increasing Events Only, -1 = Decreasing Events Only)
direction <- 0

# List of all category datasets
categories <- c(
  "Computing.Accessories_Events", "Floorcare_Events", "Headphones_Events",
  "Home.Theater.Accessories_Events", "Input.Devices_Events", "iPad_Events",
  "laptop.accessories_Events", "MacBook_Events", "Mounts_Events",
  "Printers_Events", "tablet.accessories_Events", "Traffic.Appliances_Events"
)

# Loop through each target column
for (target_col in target_cols) {
  
  # Initialize Main_Df for each target column
  Main_Df <- data.frame(Company = character())
  
  for (category in categories) {
    dataset <- get(category)
    
    # Run your correlation function
    Top20_Corr <- top_20_function(dataset, target_col, direction)
    
    # Extract company name and gain
    Companies <- substr(Top20_Corr$Feature, 1, nchar(Top20_Corr$Feature) - 3)
    Top_Df <- data.frame(Company = Companies, Gain = Top20_Corr$Gain)
    
    Top_Df <- Top_Df %>%
      group_by(Company) %>%
      summarise(Gain = sum(Gain), .groups = "drop") %>%
      arrange(desc(Gain))
    
    colnames(Top_Df)[2] <- paste0("Gain - ", category)
    
    # Merge Top_Df with Main_Df
    Main_Df <- full_join(Top_Df, Main_Df, by = "Company")
  }
  
  # Dynamically create a uniquely named data frame for each target column
  assign(paste0("Main_Df_", gsub("[^A-Za-z0-9]", "_", target_col)), Main_Df)
}
