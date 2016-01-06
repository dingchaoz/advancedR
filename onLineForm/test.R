library(shiny)
library(shinyjs)

## very useful debugging while interacting function to call before running app
##options(shiny.trace=TRUE)
#options(shiny.fullstacktrace = TRUE)
#options(shiny.error=browser)

# Array holding the mandatory filled inputs
fieldsMandatory <- c("name", "wwid","program","se","param")
# Array holding all the inputs whether filled or not to be saved
fieldsAll <- c("name","wwid","program","se","param","threshold","threshold2","th_type")

# Function to form a span, adding a * to the end, and name the span a class
labelMandatory <- function(label) {
    tagList(label,span("*", class = "mandatory_star")
)}

# CSS string, CSS an id using #, class using .before the name
appCSS <-  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }"

# Directory named Requests in which all requests csv got saved
responsesDir <- file.path("Requests")

# Get the current epoch time(unix time starting from 1970/01/01)
epochTime <- function() { as.integer(Sys.time())}
# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {format(Sys.time(), "%Y%m%d-%H%M%OS")}

# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE) # List all the files in the response dir
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) # Read all the files using read.csv function
  data <- do.call(rbind, data) # bind all data read by row to form a data frame
  data 
}

# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data)) # form the saved file name containing time stamp and a random hashed string
                                            # using sprintf
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)  # save to a csv file using write.csv
}

# usernames that are admins
adminUsers <- c("admin", "prof")


ui <- fluidPage(
  titlePanel("OBD Data Analysis Request Online Dashboard"),
  shinyjs::useShinyjs(), # call shinyjs packages
  shinyjs::inlineCSS(appCSS), # use the CSS defined in the global space 
  
  fluidRow(
    column(6,
           div(
             id = "form",
             textInput("name", labelMandatory("Name"), ""),
             textInput("wwid", labelMandatory("WWID")),
             textInput("program", labelMandatory("Program"), ""),
             textInput("se", labelMandatory("System Error")),
             textInput("param", labelMandatory("Parameter"), ""),
             textInput("threshold", "Threshold"),
             textInput("threshold2", "Second Threshold"),
             #sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
             selectInput("th_type", "Threshold Type",
                         c("",  "USL", "LSL", "Both")),
             actionButton("submit", "Submit", class = "btn-primary"),
             
             shinyjs::hidden(
               span(id = "submit_msg", "Submitting..."),
               div(id = "error",
                   div(br(), tags$b("Error: "), span(id = "error_msg"))
               )
             )
             
           ),
           shinyjs::hidden(
             div(
               id = "thankyou_msg",
               h3("Thanks, your response was submitted successfully!"),
               actionLink("submit_another", "Submit another response")
             )
           )
        ),
    column(6,
          uiOutput("adminPanelContainer") 
           )

    )

)

server <- function(input, output,session) {
  
  # Enable the Submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled <- vapply(fieldsMandatory,function(x){!is.null(input[[x]])&&input[[x]] != ""},logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    shinyjs::toggleState(id = "submit",condition = mandatoryFilled)

  })
  
  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  


  saveData <- function(data) {fileName <- sprintf("%s_%s.csv", humanTime(),digest::digest(data)) 
  write.csv(x = data, file = file.path(responsesDir, fileName),row.names = FALSE, quote = TRUE)}
  
   # action to take when submit button is clicked
  observeEvent(input$submit, 
               {
                 shinyjs::disable("submit")
                 shinyjs::show("submit_msg")
                 shinyjs::hide("error")
                 
                 # Save the data (show an error message in case of error)
                 tryCatch({
                   saveData(formData())  # save the form filled data
                   shinyjs::reset("form") # reset the form div
                   shinyjs::hide("form") # hide the form div
                   shinyjs::show("thankyou_msg") # show the thankyou_msg div
                 },
                 error = function(err) {
                   shinyjs::text("error_msg", err$message) # catch the error messge and parse it as the text of error_msg span
                   shinyjs::show(id = "error", anim = TRUE, animType = "fade") # show the id
                   },
                 finally = {
                   shinyjs::enable("submit") # make the submit button appear again and allow user to submit again
                   shinyjs::hide("submit_msg") # hide the submit_msg div
                   })
                
                })
  
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
    })
  
  
  # render the admin panel
  output$adminPanelContainer <- renderUI({
    if (!isAdmin()) return()
    
    div(
      id = "adminPanel",
      h2("Previous responses (only visible to admins)"),
      downloadButton("downloadBtn", "Download responses"), br(), br(),
      DT::dataTableOutput("responsesTable") 
    )
  })
  
  # determine if current user is admin
  isAdmin <- reactive({
    is.null(session$user) || session$user %in% adminUsers
  })    
  
  # Show the responses in the admin table
  output$responsesTable <- DT::renderDataTable(
    loadData(),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  )
  
  # Allow user to download responses
  output$downloadBtn <- downloadHandler(
    filename = function() { 
      sprintf("all-requests-form_%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE)
    }
  )

}

shinyApp(ui = ui, server = server)