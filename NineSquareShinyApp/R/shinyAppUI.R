#' Shiny app server object
#' @name shinyAppUI
#' @import shiny

shinyAppUI <- shiny::fluidPage(

    # Application title
    titlePanel("Nine Square Plot from MAGECK"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons("filetype","type of input:",choices=c("MAGECKmle Gene Summary","JACKS results")),
            fileInput(
                "GeneSummary",
                "Gene Summary or Results Table"
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
                      "Single Gene To Highlight"),
            sliderInput("axesRanges",
                        "range of x and y axes",
                        min=-6,
                        max=+6,
                        value=c(-3,3))
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
