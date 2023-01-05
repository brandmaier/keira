#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(exams)

std_text =  paste0("Hinweise: \\\\",
"\\begin{itemize}",
"\\item Die Klausur besteht aus 3 geschlossenen Fragen mit vorgegebenen Antwortm\\\"oglichkeiten (jeweils eine Antwort ist richtig) ",
"\\item Bitte pr\\\"ufen Sie vor der Abgabe, dass Sie alle Fragen auf dem Deckblatt beantwortet haben. Nur die Antworten auf dem Deckblatt werden ber\\\"ucksichtigt.",
"\\item Viel Erfolg!",
"\\end{itemize}",
"\\vspace{0.5cm}", sep="", collapse="")


page1 <- fluidPage(

shiny::textInput("title","Titel der Klausur", value="M25 - Forschungsmethodik"),
shiny::textInput("course","Modulnummer", value="M25"),
shiny::checkboxInput("showpoints", "Punktzahl anzeigen",value = TRUE),
shiny::fileInput("docx_file", "Klausur Word Dokument"),
#sliderInput("n",
#            "Anzahl von Exemplaren:",
#            min = 1,
#            max = 5000,
#            value = 1),
shiny::numericInput("n","Anzahl von Exemplaren",1,min=1),

shiny::numericInput("nq",
                    "Wieviele zufällig gezogene Fragen soll die Klausur enthalten (0=alle, aber höchstens 45)",0,
                    min=10,max=45),

shiny::numericInput("seed","Seed", value = round(runif(1,0,.Machine$integer.max)) ,min=1),

shiny::textInput("include_tags","Tags (Einschluss)", value=""),
shiny::textInput("exclude_tags","Tags (Ausschluss)", value=""),


shiny::textAreaInput("preamble",label="Begrüßungstext",
                     value=std_text,
                     cols = 80, rows=5),

#shiny::textInput("hashtag_exclude","Ausschluss", value=""),

shiny::actionButton(inputId = "do", label="Erstellen"),

NULL)


# Define UI for application that draws a histogram
ui <- fluidPage(

    tags$head(tags$script(src = "message-handler.js")),

    # Application title
    titlePanel("Keira - (K)lausur (E)rstellen (i)st (R)ichtig (A)ngenehm"),

    # Sidebar with a slider input for number of bins
    #sidebarLayout(
    #    sidebarPanel(
           # shiny::h1("keira - K.lausuren E.rstellen I.st R.ichtig A.ngenehm"),


    #    ),

       #



        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(type = "tabs",
                      tabPanel("(I) Erstellen",
                               page1

                               ),
                      tabPanel("(II) Einlesen",

                               fluidPage(

                               shiny::h1("Einlesen")
                               ,  shiny::fileInput("exam_file", "Gescannte Klausuren (als ZIP-Datei)"),
                               shiny::actionButton(inputId = "do2", label="Einlesen"),

                               )

                               ),

                      tabPanel("(III) Auswerten",
                          fluidPage(
                            shiny::h1("Auswerten"),
                            shiny::fileInput("scanned_file", "Gescannte Klausuren aus Schritt II (als ZIP-Datei)"),
                            shiny::fileInput("solution_file", "Datei mit korrekten Antworten (*.rds)"),
                            shiny::radioButtons(inputId="scoring_rule", label="Bewertungsfunktion",
                                                choices=c("none","all","false","false2"),selected = "false"),
                            shiny::checkboxInput(inputId="partial","Partielle Punkte", value=TRUE),
                            shiny::actionButton(inputId = "do3", label="Bewerten")
                            )
                      )
          )
      #     plotOutput("distPlot")
       )
    )
#)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$do3, {
    evaluate(scans=input$scanned_file$datapath,
             solutions=input$solution_file$datapath,
             partial=input$partial,
             rule=input$scoring_rule)
  })


  observeEvent(input$do2, {
    scanExam(input$exam_file$datapath)
  })

  observeEvent(input$do, {
   # session$sendCustomMessage(type = 'testmessage',
      #                        message = 'Thank you for clicking')

    set.seed(input$seed)

    # --
    print("Current path")
    print(getwd())

    include_tags <- input$include_tags
    exclude_tags <- input$exclude_tags

    if (exclude_tags=="") exclude_tags <- c() else
      exclude_tags = strsplit(exclude_tags,"\\s+")[[1]]
    if (include_tags=="") include_tags <- c() else
      include_tags = strsplit(include_tags,"\\s+")[[1]]

    #browser()

    #print(input$docx_file)
    converter(input$docx_file$datapath,subdir = "",include_tags=include_tags, exclude_tags=exclude_tags)
    print("CONVERSION DONE")
    files = list.files(path = "./",pattern = "*Rnw$")
    print(files)

    nq <- input$nq
    if (nq < length(files) && nq>0) {
      files = sample(files, size=nq, replace=FALSE)
    }

    if (nq==0 && length(files)>45) {
      files = sample(files, size=45, replace=FALSE)
    }

    generate(files = files, n=input$n, title=input$title, course = input$course,
             showpoints = input$showpoints)

    # remove files
    #unlink("*\\.Rnw")

    cat("Removed Rnw files")

  })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

run <- function() {
# Run the application
shinyApp(ui = ui, server = server)
}

run()
