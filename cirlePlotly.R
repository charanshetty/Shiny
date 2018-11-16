library(plotly)

dat <- data.frame(y=factor(c(1,1,2,2),levels=c(1,2),labels=c("GRP1","GRP2")),
                  x=c(1,2,3,4),
                  n=c(1,2,3,4),
                  grp=c("GRP1","GRP1","GRP2","GRP2"))

p1 <- plot_ly(dat) %>%
  add_markers(x = ~x, 
              y = ~y, color = ~grp, 
              marker = list(size=~I(n*10)),
              opacity = .7) %>%
  layout(showlegend=F)

dat2 <- data.frame(y=factor(c(1,1,2,2),levels=c(1,2),labels=c("GRP1","GRP2")),
                   x=c(1,2,3,4),
                   n=c(1,2,1,1),
                   grp=c("GRP1","GRP1","GRP2","GRP2"))

p<-plot_ly(dat2) %>%
  add_markers(x = ~x, 
              y = ~y,  color = ~grp, 
              marker = list(size=~I(n*10)),
              opacity = .7) %>%
  layout(showlegend=F)
p
htmlwidgets::saveWidget(p, "circles.html")

htmlwidgets::saveWidget(p1, "circles2.html")


  
p3 <- plot_ly(coords[which(coords$macid=="547384250260a45c2df11d08092050ca8dee4df2"), ])%>%
  add_markers(x=~x,y=~y,text=~distance,
               marker=list( size=~I(distance),sizemode="diameter",sizeref=nrow(img)/max(coords[["distance"]]), opacity=.4,  colorscale='Viridis',  colorbar=list(
                 title="time"
               ),color=coords[["time"]]
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
  
  


htmlwidgets::saveWidget(p3, "circles3.html")
