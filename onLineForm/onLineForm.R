library(shiny)
library(shinyjs)

fieldsMandatory <- c("name", "wwid","program","se","param")

labelMandatory <- function(label) {
  tagList(label,span("*", class = "mandatory_star")
  )}

appCSS <- ".mandatory_star {color: red; }
#error { color: red; }"


fieldsAll <- c("name","wwid","program","se","param","threshold","threshold2","th_type")
responsesDir <- file.path("Requests")
epochTime <- function() { as.integer(Sys.time())}
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")


# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}

# usernames that are admins
adminUsers <- c("admin", "prof")


ui <- fluidPage(
  titlePanel("OBD Data Analysis Request Online Dashboard"),
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  
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
  
  observe({
    mandatoryFilled <- vapply(fieldsMandatory,function(x){!is.null(input[[x]])&&input[[x]] != ""},logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    shinyjs::toggleState(id = "submit",condition = mandatoryFilled)
    
  })
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  
  
  
  saveData <- function(data) {fileName <- sprintf("%s_%s.csv", humanTime(),digest::digest(data)) 
                              write.csv(x = data, file = file.path(responsesDir, fileName),row.names = FALSE, quote = TRUE)}
  # action to take when submit button is pressed
  observeEvent(input$submit, 
{
  shinyjs::disable("submit")
  shinyjs::show("submit_msg")
  shinyjs::hide("error")
  
  tryCatch({
    saveData(formData())  
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  },
  error = function(err) {
    shinyjs::text("error_msg", err$message)
    shinyjs::show(id = "error", anim = TRUE, animType = "fade")
  },
  finally = {
    shinyjs::enable("submit")
    shinyjs::hide("submit_msg")
  })
  
})

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