
library(caret)
library(shiny)
library(LiblineaR)
library(readr)
library(ggplot2)
library(randomForest)


load("prediction_random_forest.rda") # Load saved model
source("impute_random_forest.R") #For imputation

original_data <- read.csv("original_dataset.csv")
original_data <- original_data[c('ID','ID_status','active','count_reassign','count_opening','count_updated','ID_caller','opened_by','Created_by','updated_by','type_contact','location','category_ID','user_symptom','Support_group','support_incharge','Doc_knowledge','confirmation_check','notify')]
#source("featureMapping.R")  #a function for feature engineering. 



shinyServer(function(input, output) {
options(shiny.maxRequestSize = 800*1024^2)   #Maximum file size upload limit 80MB.
                                              
  
  
output$sample_input_data_heading = renderUI({   # show only if data has been uploaded
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }
    else{
      tags$h4('Sample data')
    }
})

output$ui.action <- renderUI({
  if (is.null(input$file1)) return()
  fluidRow(
    column(6, align="center", offset = 3,
           downloadButton("downloadData",label = "'Download Predictions'"
                          #button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
           )))
})

output$sample_input_data = renderTable({    # show sample of uploaded data
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }
    else{
      input_data =  readr::read_csv(input$file1$datapath)
      input_data <- input_data[c('ID','ID_status','active','count_reassign','count_opening','count_updated','ID_caller','opened_by','Created_by','updated_by','type_contact','location','category_ID','user_symptom','Support_group','support_incharge','Doc_knowledge','confirmation_check','notify')]
      head(input_data)
    }
  })
  
predictions<-reactive({ #Pridiction on data.
    
    inFile <- input$file1
    
    if (is.null(inFile)){
      return(NULL)
    }
    else{
      withProgress(message = 'Predictions in progress. Please wait ...', {
      input_data =  readr::read_csv(input$file1$datapath)
      input_data <- input_data[c('ID','ID_status','active','count_reassign','count_opening','count_updated','ID_caller','opened_by','Created_by','updated_by','type_contact','location','category_ID','user_symptom','Support_group','support_incharge','Doc_knowledge','confirmation_check','notify')]
      #Imputation
      input_data <- as.data.frame(gsub("[[:punct:]]", NA, as.matrix(input_data))) 
      input_data <- droplevels(input_data)
      col_null <- which(colSums(is.na(input_data))!=0)
      pb = txtProgressBar(min = 0, max = length(col_null), initial = 0) 
      for (i in 1:length(col_null)) {
        if(sum(is.na(input_data))!=0){
          col_null_per <- sum(is.na(input_data[names(col_null)[i]]))/dim(input_data)[1]*100
          if(col_null_per>3){
            input_data <- impute(input_data,names(col_null)[i])
          }else{
            input_data[names(col_null)[i]][is.na(input_data[names(col_null)[i]])] <- names(which.max(table(input_data[names(col_null)[i]])))
          }
          
        }
        setTxtProgressBar(pb,i)
      }
      #End of Imputation.
      
      #Factore Variable Encoding.
      original_data['df_type'] <- 'original'
      input_data['df_type'] <- 'input'
      
      new_data <- rbind(original_data,input_data)
      new_data <- droplevels(new_data)
      for (i in 1:ncol(new_data)){
        if(class(new_data[,i])=='factor' | class(new_data[,i])=='logical' ){
          new_data[,i] <- as.numeric(new_data[,i])
        }
      }

      #Getting inpute Dataset
      new_input_data <- new_data[new_data$df_type=='input',]
      new_input_data <- new_input_data[,-c(dim(new_input_data)[2])]
      input_data <- input_data[,-c(dim(input_data)[2])]
      
      #Pridiction of input data.
      prediction = predict(train_rand_forest,new_input_data)
      head(prediction)
      input_data_with_prediction = cbind(input_data,prediction)
      input_data_with_prediction
      
      })
    }
  })
  

output$sample_prediction_heading = renderUI({  # show only if data has been uploaded
  inFile <- input$file1

  if (is.null(inFile)){
    return(NULL)
  }
  else{
    tags$h4('Sample predictions')
  }
})

output$sample_predictions = renderTable({   # the last 6 rows to show
  pred = predictions()
  head(pred)
})


#output$plot_predictions = renderPlot({   # the last 6 rows to show
#  pred = predictions()
#})


#Downloadable csv of predictions ----

output$downloadData <- downloadHandler(
  filename = function() {
    paste("input_data_with_predictions.csv")
  },
  content = function(file) {
    write.csv(predictions(), file, row.names = FALSE)
  })

})
