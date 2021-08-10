#' Shiny app server object
#' @name shinyAppUI
#' @import shiny

shinyAppUI <- shiny::fluidPage(

    # Application title
    titlePanel("Nine Square Plot from MAGECK"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput(
                "GeneSummary",
                "Gene Summary Table"
            ),
            uiOutput("xChoices"),
            uiOutput("yChoices"),
            radioButtons("NormMethod",
                         "Select normalization method",
                         choices=c("None","cell_cycle","loess"),
                         selected=c("None")),
            sliderInput("Cutoff",
                        "Cutoff:",
                        min = 0.5,
                        max = 5,
                        value = 3),
            textInput("GeneToHighlight",
                      "Single Gene To Highlight")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           fluidRow(
               column(6,plotOutput("preNormPlot")),
               column(6,plotOutput("postNormPlot"))
           ),
           uiOutput("downloadPlotButton"),
           plotOutput("nineSquarePlot"),
           fluidRow(
               column(3,
                      uiOutput("downloadTopCenterButton"),
                      tableOutput("topCenterTable")),
               column(3,
                      uiOutput("downloadMidRightButton"),
                      tableOutput("midRightTable")),
               column(3,
                      uiOutput("downloadBottomCenterButton"),
                      tableOutput("bottomCenterTable")),
               column(3,
                      uiOutput("downloadMidLeftButton"),
                      tableOutput("midLeftTable"))
           )
        )
    )
)
