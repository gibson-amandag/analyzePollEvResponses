library(shiny)
library(readr)
library(openxlsx)
library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)
library(fs)

# Define the user interface
ui <- fluidPage(
  titlePanel("Analyze PollEverywhere Responses"),
  sidebarLayout(
    sidebarPanel(
      p("Select CSVs exported from PollEverywhere for analysis"),
      a("Download example CSV Files", href = "DemoCSVs.zip"),
      fileInput("files", "Upload Files",
                multiple = TRUE,
                accept = c("text/csv", ".csv")),
      downloadButton("downloadData", "Download Workspace"),
      downloadButton("downloadPlot", "Download Plot")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Question",
                 selectInput("select_question", "Select the question:",
                             choices = NULL,
                             selected = NULL),
                 textOutput("text_question"),
                 dataTableOutput("questionsTable"),
                 plotOutput("plot_pie")),
        tabPanel("Difficult Qs",
                 dataTableOutput("difficultQs")
                 , h4("Common wrong answers")
                 , dataTableOutput("commonWrong")
        ),
        tabPanel("Student Performance",
                 dataTableOutput("studentPerformance")
        ),
        tabPanel("Student",
                 selectInput("select_student", "Select the student:",
                             choices = NULL,
                             selected = NULL),
                 dataTableOutput("studentResponses"),
                 textOutput("indivPercentage")
        ),
      )
    )
  )
)

# Define the server logic
server <- function(input, output, session){
  
  read_csv_to_df <- function(csv_file_path, index) {
    question_text <- read_lines(csv_file_path, n_max = 1)
    question_text <- str_remove_all(question_text, "^\"|\"$")
    question_text <- str_remove(question_text, ",+$")  # remove commas at the end
    # question_text <- paste0("Q", index, ": ", question_text)  # prepend index
    data_frame <- read_csv(csv_file_path, skip = 1, show_col_types = FALSE)
    data_frame$Question <- question_text
    return(data_frame)
  }
  
  data <- reactive({
    req(input$files$datapath %>% path_ext() == "csv")
    inFile <- input$files
    df <- purrr::map2_dfr(inFile$datapath, seq_along(inFile$datapath), read_csv_to_df)
    df <- df %>%
      select(-Via, -`Created At`) %>%
      rename(correct = `Correct?`, student = `Screen name`)
    updateSelectInput(session, "select_question", 
                      choices = unique(df$Question), 
                      selected = unique(df$Question)[1])
    updateSelectInput(session, "select_student", 
                      choices = unique(df$student),
                      selected = unique(df$student)[1])
    return(df)
  })
  
  studentPerformance <- reactive({
    req(data())
    df <- data() %>%
      filter(
        !is.na(correct)
      ) %>%
      group_by(student) %>%
      summarise(total_answers = n(),
                correct_answers = sum(correct == "Yes")) %>%
      arrange(
        correct_answers
      )
    return(df)
  })
  
  output$studentPerformance <- renderDataTable({
    req(studentPerformance())
    return(studentPerformance())
  })
  
  output$questionsTable <- renderDataTable({
    req(questionsTable())
    questionsTable()
  })
  
  questionsTable <- reactive({
    req(data())
    data() %>%
      filter(
        Question == input$select_question
      ) %>%
      select(
        -Question
      )
  })
  
  output$studentResponses <- renderDataTable({
    req(data())
    data() %>%
      filter(
        student == input$select_student
      )
  })
  
  difficultQs <- reactive({
    req(data())
    data() %>%
      filter(
        !is.na(correct)
      ) %>%
      group_by(Question) %>%
      summarise(total_answers = n(),
                correct_answers = sum(correct == "Yes")) %>%
      mutate(correct_rate = correct_answers / total_answers) %>% 
      arrange(correct_rate) 
  })
  
  output$difficultQs <- renderDataTable({
    req(difficultQs())
    difficultQs()
  })
  
  commonWrong <- reactive({
    req(data())
    numResponsesPerQuestion <- data() %>%
      filter(
        !is.na(correct)
      ) %>%
      group_by(Question) %>%
      summarize(total = n())
    
    common_wrong_answers <- data() %>%
      filter(correct == "No") %>%
      group_by(Question, Response) %>%
      summarise(selected = n()) %>%
      left_join(
        numResponsesPerQuestion
        , by = "Question"
      ) %>%
      mutate(
        percSelected = selected / total
      ) %>%
      arrange(desc(percSelected))
    return(common_wrong_answers)
  })
  
  output$commonWrong <- renderDataTable({
    req(commonWrong())
    commonWrong()
    
  })
  
  # Define output for text_question
  output$indivPercentage <- renderText({
    req(data())
    df <- studentPerformance() %>%
      filter(
        student == input$select_student
      )
    return(
      paste0(df$student[1], " answered ", df$correct_answers[1], "/", df$total_answers[1], " questions correctly (", df$correct_answers[1]/df$total_answers[1] * 100, "%)" )
    )
  })
  
  # Define output for text_ans 
  output$text_percentage <- renderText({
    # Your code here
  })
  
  piePlot <- reactive({
    req(data())
    subset_df <- data() %>% 
      filter(Question == input$select_question)
    
    # Count the number of each type of Response
    response_counts <- subset_df %>%
      group_by(Response, correct) %>%
      summarise(count = n(), .groups = "drop") %>%
      arrange(correct) %>%
      mutate(correct2 = ifelse(correct == 'No', paste0(correct, row_number()), correct),  # Create correct2 column #AGG - no needs to be first
             correct2 = ifelse(is.na(correct), paste0("NA", row_number()), correct2))    # Handle NA cases
    
    # Create different color vectors for different 'correct2' cases
    if (all(is.na(response_counts$correct))) {
      color_vector <- setNames(rainbow(nrow(response_counts)), response_counts$correct2)
    } else {
      # Else use original defined colors
      if(all(response_counts$correct == "Yes")){
        color_vector <- c('Yes' = 'lightblue')
      } else{
        color_vector <- c('Yes' = 'lightblue', setNames(colorRampPalette(c('red', 'darkred'))(sum(response_counts$correct == 'No')), paste0('No', 1:sum(response_counts$correct == 'No'))))
      }
    }
    
    # Create pie chart with labels
    req(color_vector)
    plot <- ggplot(response_counts, aes(x="", y=count, fill=correct2)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      theme_void() +
      scale_fill_manual("Response", values = color_vector, labels = function(x) response_counts$Response[match(x, response_counts$correct2)]) +   # Modified legend text to "Response"
      ggtitle(str_wrap(input$select_question, width = 60)) +
      geom_text(aes(label = paste0(round((count/sum(count))*100),"%")), position = position_stack(vjust = 0.5)) +
      theme(
        text = element_text(size = 20)
      )
    return(plot)
  })
  
  output$plot_pie <- renderPlot({
    req(piePlot())
    piePlot()
  })
  
  # Define output for downloadData
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("workspace", ".xlsx", sep = "")
    },
    content = function(file) {
      req(data())
      wb <- createWorkbook()
      
      # Adds cleaned combined data frame as last sheet
      addWorksheet(wb, "Combined Data")
      writeData(wb, "Combined Data", data() %>% arrange(student, correct))
      
      # Add each analysis to a separate sheet in workbook
      addWorksheet(wb, "Performance by Student")
      writeData(wb, "Performance by Student", studentPerformance())
      
      addWorksheet(wb, "Questions by Difficulty")
      writeData(wb, "Questions by Difficulty", difficultQs())
      
      addWorksheet(wb, "Common Wrong Answers")
      writeData(wb, "Common Wrong Answers", commonWrong())
      
      
      # Add data frame to separate sheets in the new workbook
      for(i in seq_along(unique(data()$Question))) {
        sheet_name <- paste("Sheet", i, sep = "_")
        addWorksheet(wb, sheet_name)
        question = unique(data()$Question)[[i]]
        filteredDF <- data() %>%
          filter(
            Question == question
          ) %>%
          select(
            -Question
          )
        writeData(wb, sheet_name, x = question, startRow = 1, startCol = 1)
        writeData(wb, sheet_name, x = data(), startRow = 3)
        
        # Set column width and wrap text for column A in each sheet
        setColWidths(wb, sheet_name, cols=1, widths="30")  # set the width of column A to 30
      }
      
      # Save workbook to a file
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # Define output for downloadPlot
  output$downloadPlot <- downloadHandler(
    filename = function(){
      paste("piechart", ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = piePlot(), device = "png", width = 8, height = 8) # adjust width and height as needed
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)