shinyServer(function(input, output, session) {
  # Logic to subset the data based on the macIds selection
  plotData <- reactive({
    out <- dataSubset
    
    
    # Subset the original data if the place selection is not empty
    if(input$macIdSel != ""){
      out <- out[macid == input$macIdSel]
    }
    if(input$errorTypeSel !=""){
      out <- out[error == input$errorTypeSel]
    }
    out$y=nrow(img)-out$y
    
    out = out[which(out$time<input$wrapCutoffTime),]
    
   
    # Add the time wrap factor
    out[, timeHalf := factor(ifelse(time<input$wrapCutoffTime,
                                    "First time part", "Last time part"))]
    
    out
  })
  circleData <- reactive({
    out <- coords
    # Subset the original data if the place selection is not empty
    if(input$macIdSel != ""){
      out <- out[macid == input$macIdSel]
    }

    out$y=nrow(img)-out$y
    
    out = out[which(out$time<input$wrapCutoffTime),]
    
    out

  out
  })
  
  # Previous macid logic
  observeEvent(input$prevMacid,{
    currentId <- which(input$macIdSel == macIds)
    newId <- max(c(1, currentId - 1))
    
    # Update place
    updateSelectInput(session, "macIdSel", selected = macIds[newId])
  })
  
  # Next macid logic
  observeEvent(input$nextMacid,{
    currentId <- which(input$macIdSel == macIds)
    newId <- min(c(length(macIds), currentId + 1))
    
    # Update place
    updateSelectInput(session, "macIdSel", selected = macIds[newId])
  })
  
  # Previous analysis variable logic
  observeEvent(input$prevAnalysisVar,{
    currentId <- which(input$analysisVar == analysisVars)
    newId <- max(c(1, currentId - 1))
    
    # Update analysis variable
    updateSelectInput(session, "analysisVar", selected = analysisVars[newId])
  })
  
  # Next analysis variable logic
  observeEvent(input$nextAnalysisVar,{
    currentId <- which(input$analysisVar == analysisVars)
    newId <- min(c(length(analysisVars), currentId + 1))
    
    # Update analysis variable
    updateSelectInput(session, "analysisVar", selected = analysisVars[newId])
  })
  
  output$circlePlotly <- renderPlotly({
    if(input$xyType == "Observations" && input$macIdSel == "") return()
    circleData <- circleData()
  #  print(circleData$distance)
  #  print(circleData$macid)
    if(input$xyType == "Observations"){
      p3 <- plot_ly( circleData)%>%
        add_markers(x=~x,y=~y,text=~distance,sizes=c(min(coords[["distance"]]),max(coords[["distance"]])),
                    marker=list( size=~I(distance),sizemode="diameter",sizeref=nrow(img)/max(coords[["distance"]])
, opacity=.3,  colorscale='Viridis',  colorbar=list(
                      title="distance"
                    ),color=coords[["distance"]]
                    )) %>%
        layout(     xaxis = list(range = c(0,ncol(img))), 
                    yaxis = list(range = c(0, nrow(img))),
                    images = list(
                      list(
                        source =  paste('data:image/png;base64', txt, sep=','),
                        xref = "x",
                        yref = "y",
                        x=0,
                        y=nrow(img),
                        sizex=ncol(img),
                        sizey=nrow(img),
                        sizing = "stretch",
                        opacity = 1.0,
                        layer = "below"
                      )
                    )
        )
      
    }
  })
  
  # Generate the x-y plot for the plot data
  output$xyPlotly <- renderPlotly({
    if(input$xyType == "Observations" && input$macIdSel == "") return()
    plotData <- plotData()

    # Generate the ggplot based on the plot type selection
    if(input$xyType == "Density"){

      p <- ggplot(plotData, aes_string(x="x", y="y", z=input$analysisVar)) +
        stat_summary_2d(fun = mean, bins = input$nbXYBins)
      
      if(!is.factor(plotData[[input$analysisVar]])){
       p <- p + scale_color_viridis()
      }
      # Optionally wrap vs time
      if(input$wrapTime){
        p <- p +
          facet_wrap(~timeHalf,nrow=2)
      }
      
      ggplotly(p)
    } else{
 
      if(!is.factor(plotData[[input$analysisVar]])){
        p <- plot_ly(plotData,x=~x,y=~y,text = plotData[[input$analysisVar]], type = 'scatter', mode = 'markers',
                     marker=list( size=6 , opacity=1,  colorscale='Viridis',  colorbar=list(
                       title=input$analysisVar
                     ),color=plotData[[input$analysisVar]]
        )) %>%
          layout(     xaxis = list(range = c(0,ncol(img))), 
                      yaxis = list(range = c(0, nrow(img))),
            images = list(
              list(
                source =  paste('data:image/png;base64', txt, sep=','),
                xref = "x",
                yref = "y",
                x=0,
           y=nrow(img),
                sizex=ncol(img),
                sizey=nrow(img),
                sizing = "stretch",
                opacity = 1.0,
                layer = "below"
              )
            )
          )
        
      }

    p
    }
  })
  
  # Reactive calculation of the maximum number of target bins
  maxTargetBins <- reactive({
    if(input$analysisVar %in% c("hour", "day")){
      out <- 1 + max(plotData()[[input$analysisVar]]) -
        min(plotData()[[input$analysisVar]])
    } else{
      out <- Inf
    }
    
    out
  })
  
  # Generate the density plot for the plot data
  output$densityPlotly <- renderPlotly({
    plotData <- plotData()
    
    # Generate the ggplot based on the plot type selection
    if(input$densityType == "Histogram"){
      p <- ggplot(plotData, aes_string(input$analysisVar)) +
        geom_histogram(bins = min(c(maxTargetBins(),
                                    input$nbDensityBins)))
    } else{
      p <- ggplot(plotData, aes_string(input$analysisVar)) +
        geom_density()
    }
    
    # Restrict x axis for accuracy density plot
    if(input$analysisVar == "accuracy"){
      p <- p +
        xlim(c(0,input$maxAcDensPlot))
    }
    
    # Optionally wrap vs time
    if(input$wrapTime){
      p <- p +
        facet_wrap(~timeHalf,nrow=2)
    }
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Generate the density plot for the plot data
  output$accuracyDensityPlotly <- renderPlotly({
    plotData <- plotData()
    
    if(input$accuracyDensityRemoveOutliers){
      # Calculate outliers 
      values <- plotData[[input$analysisVar]]
      outliers <- values %in% boxplot.stats(values, coef=2)$out
      plotData <- plotData[!outliers,]
    }
    
    # Generate the ggplot based on the plot type selection
    if(input$accuracyDensityType == "Histogram"){
      intercepts <- plotData[,median(as.numeric(get(input$analysisVar))),
                             by=accuracyGroup]
      p <- ggplot(plotData, aes_string(input$analysisVar)) +
        geom_histogram(bins = min(c(maxTargetBins(),
                                    input$nbDensityBins))) +
        geom_vline(data=intercepts, aes(xintercept = V1), colour="green")
    } else{
      p <- ggplot(plotData, aes_string(input$analysisVar)) +
        geom_density()
    }
    
    # Wrap vs accuracy group
    p <- p +
      facet_wrap(~accuracyGroup, ncol=1)
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Generate the custom comparison plot
  output$comparisonPlotly <- renderPlotly({
    plotData <- plotData()
    
    if(input$compPlotType == "Scatter"){
      p <- ggplot(plotData, aes_string(x=input$analysisVar,
                                       y=input$comparisonVar,
                                       col=input$colComparisonVar)) +
        geom_point() +
        scale_color_viridis()
    } else{
      plotData[,CompVar := factor(plotData[[input$analysisVar]])]
      p <- ggplot(plotData, aes_string(x="CompVar",
                                       y=input$comparisonVar,
                                       fill="CompVar")) +
        geom_violin() +
        xlab(input$analysisVar) +
        theme(legend.position="none")
    }
    
    # Optionally wrap vs time
    if(input$wrapTime){
      p <- p +
        facet_wrap(~timeHalf,nrow=2)
    }
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Generate a data table of the plot data
  output$plotDataTable <- renderDataTable({
    out <- plotData()
    
    out
  }, options = list(pageLength = 10,
                    lengthMenu = list(c(5, 10, 20, -1),
                                      c('5', '10', '20', 'All'))),
  filter = "top"
  )
})
