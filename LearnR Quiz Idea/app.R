library(shiny)
library(dplyr)
library(data.table)


#----------------------------------------------------
# FUNCTIONS
#----------------------------------------------------
Question_Reformat <- function(questions) {
  # Sort out any prefixes etc
  questions$CorrectAnswerCol <- NA
  option_cols <- grep("Option", names(questions), value = TRUE)

  questions <- questions[, c("Set",
                             "Question",
                             "CorrectAnswerCol",
                             "CorrectAnswer",
                             option_cols)]

  # Identify option columns dynamically
  option_loc <- grep("Option", names(questions), value = FALSE)
  option_cols <- grep("Option", names(questions), value = TRUE)
  option_name <- unlist(lapply(strsplit(option_cols, "Option"), "[", 2))
  
    
  # Add the A| .. B| to each question option
  for (col in 1:length(option_loc)) {
    questions[, option_loc[col]] <- paste(option_name[col], 
                                          questions[, option_loc[col]], 
                                          sep ="| ")
  }
  

  
  # Find the 'correct' column and adjust the CorrectAnswer column
  for (n in 1:nrow(questions)) {
    questions$CorrectAnswerCol[n] <- option_loc[grep(questions$CorrectAnswer[n], option_name)]
    questions$CorrectAnswer[n]    <- questions[n, questions$CorrectAnswerCol[n]]
  }
  return(questions)
  
}

#-----------------------------------------------------------------------
# QUESTIONS
# Read the CSV file  & Convert to the necessary format
#-----------------------------------------------------------------------
#---Sample questions and codes----------------------------------------
# 
# Excelquestions <- data.frame(
#   Set = c("Cats", "Cats", 2, 2),
#   Question = c(
#     "What is a cat?",
#     "Do cats purr?",
#     "What is a tree?",
#     "Is green a color?"
#   ),
#   CorrectAnswer = c("B", "A", "A", "B"),
#   OptionA = c("Fire", "TRUE", "Plant", "Nope"),
#   OptionB = c("Furry animal", "FALSE", "Spaceship", "Yep")
# )
  
  
#---Read the CSV file  & Convert to the necessary format----------------  
 Excelquestions <- as.data.frame(fread("questions.csv"))
 questions <- Question_Reformat(Excelquestions)

 
#-----------------------------------------------------------------------
# CORRECT ANSWER CODES
#-----------------------------------------------------------------------
 codes <- c("CODE1", "CODE2", "CODE3", "CODE4")

#-----------------------------------------------------------------------
# DEFINE UI
#-----------------------------------------------------------------------
 ui <- fluidPage(
  titlePanel("Random Question Quiz"),
  uiOutput("quiz_ui"),
  textOutput("feedback"),
  uiOutput("button_ui"),
  uiOutput("results_ui")
)

 #-----------------------------------------------------------------------
 # DEFINE SERVER LOGIC
 #-----------------------------------------------------------------------
 server <- function(input, output, session) {
   #-----------------------------------------------------------------------
   # Function to pull a random question from a specific set
   #-----------------------------------------------------------------------
   get_random_question <- function(set_name) {
     questions %>%
       filter(Set == set_name) %>%
       sample_n(1)
   }
   
   #-----------------------------------------------------------------------
   # Reactive values for questions and codes
   #-----------------------------------------------------------------------
   random_q1 <- reactiveVal(NULL)
   random_q2 <- reactiveVal(NULL)
   available_codes <- reactiveVal(codes)
   quiz_submitted <- reactiveVal(FALSE)
   
   #-----------------------------------------------------------------------
   # Function to update questions
   #-----------------------------------------------------------------------
   update_questions <- function() {
     random_q1(get_random_question("Cat"))
     random_q2(get_random_question("Tree"))
   }
   
   #-----------------------------------------------------------------------
   # Initialize questions
   #-----------------------------------------------------------------------
   observe({
     update_questions()
   })
   
   #-----------------------------------------------------------------------
   # Render quiz UI
   #-----------------------------------------------------------------------
   output$quiz_ui <- renderUI({
     if (quiz_submitted()) {
       return(NULL) # Hide quiz UI if the quiz has been submitted
     }
     
     tagList(
       h3(random_q1()$Question),
       radioButtons(
         "q1",
         "",
         choices = c(random_q1()$OptionA, random_q1()$OptionB)
       ),
       
       h3(random_q2()$Question),
       radioButtons(
         "q2",
         "",
         choices = c(random_q2()$OptionA, random_q2()$OptionB)
       )
     )
   })
   
   # Render buttons UI
   output$button_ui <- renderUI({
     if (quiz_submitted()) {
       tagList(actionButton("try_again", "Try Again"))
     } else {
       tagList(actionButton("submit", "Submit"))
     }
   })
   
   # Handle quiz submission
   observeEvent(input$submit, {
     # Debug: Print inputs and correct answers
     print(paste("Input q1:", input$q1))
     print(paste("Correct q1:", random_q1()$CorrectAnswer))
     print(paste("Input q2:", input$q2))
     print(paste("Correct q2:", random_q2()$CorrectAnswer))
     
     correct1 <- input$q1 == random_q1()$CorrectAnswer
     correct2 <- input$q2 == random_q2()$CorrectAnswer
     
     # Calculate score
     score <- sum(c(correct1, correct2))
     
     if (correct1 && correct2) {
       codes_list <- available_codes()
       if (length(codes_list) > 0) {
         pass_code <- codes_list[1]
         available_codes(codes_list[-1])
         feedback_text <- paste("Congratulations! Your unique code is:", pass_code)
       } else {
         feedback_text <- "No more codes available."
       }
     } else {
       feedback_text <- "Some answers are incorrect. Please try again."
     }
     
     # Display results and feedback
     output$feedback <- renderText(feedback_text)
     output$results_ui <- renderUI({
       tagList(
         h1("Quiz Results:"),
         h3(""),
         
         h3(paste("Score: ", score, " out of 2")),
         h3(""),
         
         p(paste(
           "Question 1: ", random_q1()$Question
         )),
         p(paste("Your Answer: ", input$q1)),
         p(paste(
           "Correct Answer: ", random_q1()$CorrectAnswer
         )),
         h3(""),
         
         p(paste(
           "Question 2: ", random_q2()$Question
         )),
         p(paste("Your Answer: ", input$q2)),
         p(paste(
           "Correct Answer: ", random_q2()$CorrectAnswer
         ))
       )
     })
     
     # Mark quiz as submitted
     quiz_submitted(TRUE)
   })
   
   # Handle try again button
   observeEvent(input$try_again, {
     # Generate new questions for the next attempt
     update_questions()
     # Clear previous feedback and inputs
     output$feedback <- renderText("")
     output$results_ui <- renderUI(NULL)
     updateRadioButtons(session, "q1", selected = NULL)
     updateRadioButtons(session, "q2", selected = NULL)
     # Reset quiz submitted state
     quiz_submitted(FALSE)
   })
 }
 
 # Run the application
 shinyApp(ui = ui, server = server)
 