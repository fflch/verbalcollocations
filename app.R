library(shiny)
  library(stringr)
  library(readtext)
  library(fst)
  library(DT)
  library(data.table)
  SafeEncodeUTF8 <- function(string){
  paste(utf8ToInt(string), collapse = "-")
  }

  ui <- fluidPage(
    titlePanel("English-Portuguese Dictionary of Verbal Collocations "),
        # actionButton("stop","stop preview"),
        tabsetPanel(
    tabPanel("Home",
           includeMarkdown("Home.Rmd")
    ),
    tabPanel("Dictionary",
    sidebarLayout(
        sidebarPanel(width=3,
          selectInput("searchBy", "search by", choices = NULL),

          textInput("headwordText",
                           "input headword",
                            value="BALLOON, RELEASE A"),
          #actionBttn("go","go"),
          selectInput("headword", "filter matches", choices = "no matches")

        ),
        mainPanel(width=9,


tags$br(),

tags$br(),

tags$br(),

htmlOutput('title_english'),

htmlOutput('english'),

tags$br(),

tags$br(),

tags$br(),

htmlOutput('title_englishexamples'),

htmlOutput('englishexamples'),

tags$br(),

tags$br(),

tags$br(),

htmlOutput('title_portuguese'),

htmlOutput('portuguese'),

tags$br(),

tags$br(),

tags$br(),

htmlOutput('title_portugueseexamples'),

htmlOutput('portugueseexamples'),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                )
               )
             ),

      tabPanel("Preface",
           fluidRow(
             column(width=3,""),
             column(width=6,
                    includeMarkdown("Preface.Rmd")
             ),
             column(width=3,""),
         )
    ),

         )
         )

       server <- function(input, output,session) {

              if(file.exists("./Media/")){
              addResourcePath(prefix = "tmpuser", directoryPath = "./Media/")
              addResourcePath(prefix = "AltText", directoryPath = "./AltText/")

              }
            headwords <- read.fst("./data/headwords.fst")

            searchOptions <- colnames(headwords)
            searchOptions <- searchOptions[!str_detect(searchOptions, "^file$|NoDiacritics$")]

             updateSelectInput(session, "searchBy", choices=searchOptions)

            observeEvent(c(input$searchBy, input$headwordText),{


         if(!is.null(input$headwordText) && nchar(input$headwordText)>0){
                 CHOICES <- c(headwords[[input$searchBy]][grep(paste0("(^|\\s)",input$headwordText,"(\\s|$)"),headwords[[paste0(input$searchBy,"NoDiacritics")]], ignore.case=T)],
                           headwords[[input$searchBy]][grep(paste0("(^|\\s)",input$headwordText,"(\\s|$)"),headwords[[input$searchBy]])])
              }else{
                CHOICES <- "no matches"
              }

              if(length(unique(CHOICES))<length(CHOICES)){
                CHOICES <- unlist(lapply(unique(CHOICES), function(x) paste(x, 1:length(headwords$file[headwords[[input$searchBy]]==x]), sep="__") ))
              }else{
                CHOICES <- unique(CHOICES)
              }
             updateSelectInput(session, "headword", choices=CHOICES )

            })


          observeEvent(c(input$headword,input$searchBy),{
          headIndex <- unlist(str_split(input$headword, "__"))
              if(headIndex[1]!="no matches" && length(headwords$file[headwords[[input$searchBy]]==headIndex[1]])>0){
                if(length(headIndex)==1){
                  EntryData <- readRDS(paste0("./data/", headwords$file[headwords[[input$searchBy]]==headIndex[1]]))
                }else{
                  EntryData <- readRDS(paste0("./data/", headwords$file[headwords[[input$searchBy]]==headIndex[1]][as.numeric(headIndex[2])]))
                }
                       }else{
            EntryData <- NULL
            print(paste("no data matching ",input$headword))
            }
          if(!is.null(EntryData)){


output$title_english <- renderText({
          '<font size=\'+2\' color=\'steelblue\'>english</font>'
                           })

output$english <- renderText({
        paste0("<font size='+1'><b>",
          gsub('
','<br />',EntryData[[1]][['english']][[1]]),
          "</b></font>"
                         )  })

output$title_englishexamples <- renderText({
          '<font size=\'+2\' color=\'steelblue\'>english examples</font>'
                           })

output$englishexamples <- renderText({
        paste0("<font size='+1'>",
          gsub('
','<br />',EntryData[[1]][['english examples']][[1]]),
          "</font>"
                         )  })

output$title_portuguese <- renderText({
          '<font size=\'+2\' color=\'steelblue\'>portuguese</font>'
                           })

output$portuguese <- renderText({
        paste0("<font size='+1'><b>",
          gsub('
','<br />',EntryData[[1]][['portuguese']][[1]]),
          "</b></font>"
                         )  })

output$title_portugueseexamples <- renderText({
          '<font size=\'+2\' color=\'steelblue\'>portuguese examples</font>'
                           })

output$portugueseexamples <- renderText({
        paste0("<font size='+1'>",
          gsub('
','<br />',EntryData[[1]][['portuguese examples']][[1]]),
          "</font>"
                         )  }) }
               })
          observeEvent(input$stop,{
            stopApp()
          })
               }
               shinyApp(ui = ui, server = server)

