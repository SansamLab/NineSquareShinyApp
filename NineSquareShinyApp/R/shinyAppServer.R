#' Shiny app server function
#' @name shinyAppServer
#' @param input provided by shiny
#' @param output provided by shiny
#' @import MAGeCKFlute
#' @import ggplot2
#' @import ggrepel
#' @import reshape


shinyAppServer <- function(input, output) {
    options(shiny.maxRequestSize=30*1024^2)
    gdata2 <- reactive({
        req(input$GeneSummary)
        df <- read.table(input$GeneSummary$datapath,header=T)
        if(input$filetype=="MAGECKmle Gene Summary"){
            MAGeCKFlute::ReadBeta(df)
        }else{
            df
        }

    })

    betaNames <- reactive({
        req(gdata2())
        names(gdata2())[-1]
    })

    gdata_cc <- reactive({
        req(gdata2())
        if(input$NormMethod == "None"){
            gdata2()
        } else {
            MAGeCKFlute::NormalizeBeta(gdata2(),
                                       samples=c(input$xChoice, input$yChoice),
                                       method=input$NormMethod)
        }
    })

    gdata_ccForPlot <- reactive({
        req(gdata_cc())
        df <- gdata_cc()
        df$Control = rowMeans(df[,input$xChoice, drop = FALSE])
        df$Treatment = rowMeans(df[,input$yChoice, drop = FALSE])
        df$Diff <- df$Treatment - df$Control
        df
    })

    nineSquarePlot <- reactive({
        req(gdata_ccForPlot())
        df <- gdata_ccForPlot()
        plt <- MAGeCKFlute::SquareView(df,
                                       label = "Gene",
                                       ctrlname=input$xChoice,
                                       treatname=input$yChoice,
                                       x_cutoff = MAGeCKFlute::CutoffCalling(df$Control, input$Cutoff),
                                       y_cutoff = MAGeCKFlute::CutoffCalling(df$Treatment, input$Cutoff),
                                       slope = MAGeCKFlute::CutoffCalling(df$Diff, input$Cutoff),
                                       intercept = MAGeCKFlute::CutoffCalling(df$Diff, input$Cutoff),
                                       groups = c("midleft", "topcenter", "midright", "bottomcenter"),
                                       genelist = c("Rif1"))
        plt <- plt + ggrepel::geom_label_repel(data = subset(plt$data, Gene == input$GeneToHighlight),ggplot2::aes(label=Gene)) + ggplot2::geom_point(data=subset(plt$data,Gene==input$GeneToHighlight),ggplot2::aes(x=Control,y=Treatment),color="black")
        plt
    })

    getGenesFromNineSqPlot <- function(group){
        p1 <- nineSquarePlot()
        groups = p1$data$group
        p1$data$Gene[which(groups==group)]
    }

    topCenter <- reactive({
        req(nineSquarePlot())
        data.frame("topcenter"=getGenesFromNineSqPlot("topcenter"))
    })
    bottomCenter <- reactive({
        req(nineSquarePlot())
        data.frame("bottomcenter"=getGenesFromNineSqPlot("bottomcenter"))
    })
    midLeft <- reactive({
        req(nineSquarePlot())
        data.frame("midleft"=getGenesFromNineSqPlot("midleft"))
    })
    midRight <- reactive({
        req(nineSquarePlot())
        data.frame("midright"=getGenesFromNineSqPlot("midright"))
    })

    output$xChoices <- renderUI({
        req(betaNames())
        radioButtons("xChoice",
                     "Choose Control (X Axis) Beta",
                     betaNames(),
                     selected=betaNames()[1])
    })

    output$yChoices <- renderUI({
        req(betaNames())
        radioButtons("yChoice",
                     "Choose Treatment (Y Axis) Beta",
                     betaNames(),
                     selected=betaNames()[2])
    })


    output$topCenterTable <- renderTable(topCenter())
    output$bottomCenterTable <- renderTable(bottomCenter())
    output$midLeftTable <- renderTable(midLeft())
    output$midRightTable <- renderTable(midRight())

    #to compare density of the starting betas
    output$preNormPlot <- renderPlot({
        req(gdata2())
        req(input$xChoice)
        df <- reshape::melt(gdata2())
        df <- df[grep(paste(input$xChoice,input$yChoice,sep="|"),df$variable),]
        ggplot2::ggplot(data=df,
                        ggplot2::aes(x=value,
                                     color=variable)) +
            ggplot2::geom_density() +
            ggplot2::ggtitle("Before Normalization") +
            ggplot2::xlab("beta score")
    })

    #to compare density of the normalized betas
    output$postNormPlot <- renderPlot({
        req(gdata_cc())
        req(input$xChoice)
        df <- reshape::melt(gdata_cc())
        df <- df[grep(paste(input$xChoice,input$yChoice,sep="|"),df$variable),]
        ggplot2::ggplot(data=df,
                        ggplot2::aes(x=value,
                                     color=variable)) +
            ggplot2::geom_density() +
            ggplot2::ggtitle("After Normalization") +
            ggplot2::xlab("beta score")
    })

    output$nineSquarePlot <- renderPlot({
        req(nineSquarePlot())
        print(nineSquarePlot())
    })

    output$downloadPlot <- downloadHandler(
        filename = function() { "plot.pdf" },
        content = function(file) {
            ggplot2::ggsave(file, plot = nineSquarePlot(), device = "pdf")
        }
    )

    output$downloadPlotButton <- renderUI({
        req(nineSquarePlot())
        downloadButton("downloadPlot",
                       "Download 9Square Plot")
    })

    output$downloadTopCenter <- downloadHandler(
        filename = function(){"topCenter.txt"},
        content = function(fname){
            write.csv(topCenter(), fname, quote=F, row.names = F, col.names = F)
        }
    )

    output$downloadTopCenterButton <- renderUI({
        req(topCenter())
        downloadButton("downloadTopCenter",
                       "Top Center")}
    )

    output$downloadBottomCenter <- downloadHandler(
        filename = function(){"bottomCenter.txt"},
        content = function(fname){
            write.csv(bottomCenter(), fname, quote=F, row.names = F, col.names = F)
        }
    )

    output$downloadBottomCenterButton <- renderUI({
        req(bottomCenter())
        downloadButton("downloadBottomCenter",
                       "Bottom Center")}
    )

    output$downloadMidLeft <- downloadHandler(
        filename = function(){"midLeft.txt"},
        content = function(fname){
            write.csv(midLeft(), fname, quote=F, row.names = F, col.names = F)
        }
    )

    output$downloadMidLeftButton <- renderUI({
        req(midLeft())
        downloadButton("downloadMidLeft",
                       "Mid Left")}
    )

    output$downloadMidRight <- downloadHandler(
        filename = function(){"midRight.txt"},
        content = function(fname){
            write.csv(midRight(), fname, quote=F, row.names = F, col.names = F)
        }
    )

    output$downloadMidRightButton <- renderUI({
        req(midRight())
        downloadButton("downloadMidRight",
                       "Mid Right")}
    )



}

