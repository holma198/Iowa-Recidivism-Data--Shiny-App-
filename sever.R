library(shiny)
library(scales)

#Be sure to include the iowaRecidivism csv in the folder#

iowaR = read.table(file="iowaRecidivism.csv",header=TRUE,sep=',', 
                   na.strings = c("", " ","N/A", "N/A -  ", "N/A - ", "N/A-"))

shinyServer(function(input, output) {
  data = iowaR
  
  output$recidivismDistPlot <- renderPlot({
    if(input$districtDropD != "All") {
    plot(iowaR$Recidivism...Return.to.Prison[iowaR$Main.Supervising.District == input$districtDropD], xlab="Return to Prison?",
         ylab="Number of Inmates", col=c("green3","red3"), main=input$districtDropD)
    }
    else {
      plot(iowaR$Recidivism...Return.to.Prison, xlab="Return to Prison?",
           ylab="Number of Inmates", col=c("green3","red3"), main=input$districtDropD)
    }
  })
  
  propRelease<-as.data.frame.matrix(prop.table(t(table(iowaR$Release.Type, iowaR$Main.Supervising.District)), 1))
  multAllBy100<-function(vectorX) {
    for(i in 1:length(vectorX)) {
      vectorX[i]<-100*(vectorX[i])
    }
    return(vectorX)
  }
  for(i in 1:ncol(propRelease)) {
    propRelease[,i]<-multAllBy100(propRelease[,i])
  }
  output$releaseTypePlot <- renderPlot({
    # validation in shiny -- https://shiny.rstudio.com/articles/validation.html
    validate(
      need(input$ReleaseDropD != "All", "Please select a Release Type")
    )
    barplot(t(propRelease[input$ReleaseDropD]), beside=TRUE,
            col=c("gray48","gray48","gray48","gray48","red4","gray48","gray48","gray48","green4","green4"),
            xlab = "Supervising District", ylab = "Percentage of Release Type in district", main=input$ReleaseDropD)
  })
  

  output$releaseRecidPlot <- renderPlot({
    if(input$ReleaseDropD != "All") {
    plot(iowaR$Recidivism...Return.to.Prison[iowaR$Release.Type == input$ReleaseDropD], xlab="Return to Prison?",
         ylab="Number of Inmates", col=c("green3","red3"), main=input$ReleaseDropD)
    } else {
      plot(iowaR$Recidivism...Return.to.Prison, xlab="Return to Prison?",
           ylab="Number of Inmates", col=c("green3","red3"), main=input$ReleaseDropD)
    }
  })
  
  output$offenseTypeVrecidivism <- renderPlot({
    if(input$OffenseDropD != "All") {
      plot(iowaR$Recidivism...Return.to.Prison[iowaR$Convicting.Offense.Subtype == input$OffenseDropD], xlab="Return to Prison?",
           ylab="Number of Inmates", col=c("green3","red3"), main=input$OffenseDropD)
    } else {
      plot(iowaR$Recidivism...Return.to.Prison, xlab="Return to Prison?",
           ylab="Number of Inmates", col=c("green3","red3"), main=input$OffenseDropD)
    }
  })
  
  output$districtVcrime <- renderPlot({
    
    validate(
      need(input$OffenseDropD != "All", "Please select an Offense Type")
    )
    
    # http://www.statmethods.net/graphs/pie.html pie chart info 
    offenseTable<-table(iowaR$Main.Supervising.District[iowaR$Convicting.Offense.Subtype == input$OffenseDropD])
    offensePct <- round(offenseTable/sum(offenseTable)*100)
    offenseLabels<-labels(offenseTable)[[1]]
    offenseLabels<-paste(offenseLabels, offensePct)
    offenseLabels<-paste(offenseLabels, "%",sep=" ")
    pie(offenseTable,labels = offenseLabels, col=c("gray48","gray48","gray48","gray48","red4","gray48","gray48","gray48","green4","green4"),
        main=cbind(input$OffenseDropD, " Offenses by district"))
    
  })
  
  output$crimePercentByDistrict <- renderPlot({
    
    propOffense<-as.data.frame.matrix(prop.table(t(table(iowaR$Convicting.Offense.Subtype, iowaR$Main.Supervising.District)), 1))
    for(i in 1:ncol(propOffense)) {
      propOffense[,i]<-multAllBy100(propOffense[,i])
    }
    
    output$crimePercentByDistrict <- renderPlot({
      validate(
        need(input$OffenseDropD != "All", "Please select an Offense Type")
      )
      barplot(t(propOffense[input$OffenseDropD]), beside=TRUE,
              col=c("gray48","gray48","gray48","gray48","red4","gray48","gray48","gray48","green4","green4"),
              xlab = "Supervising District", ylab = "Percentage of Offense Types in district", main=input$OffenseDropD)
    })
    
  })
  
  
  
  output$genOffenseTypeVrecidivism <- renderPlot({
    
    if(input$GenOffenseDropD != "All") {
      plot(iowaR$Recidivism...Return.to.Prison[iowaR$Convicting.Offense.Type == input$GenOffenseDropD], xlab="Return to Prison?",
           ylab="Number of Inmates", col=c("green3","red3"), main=input$GenOffenseDropD)
    } else {
      plot(iowaR$Recidivism...Return.to.Prison, xlab="Return to Prison?",
           ylab="Number of Inmates", col=c("green3","red3"), main=input$GenOffenseDropD)
    }
    
  })
  
  output$genDistrictVcrime <- renderPlot({
    
    validate(
      need(input$GenOffenseDropD != "All", "Please select a General Offense Type")
    )
    
    # http://www.statmethods.net/graphs/pie.html pie chart info 
    offenseTable<-table(iowaR$Main.Supervising.District[iowaR$Convicting.Offense.Type == input$GenOffenseDropD])
    pie(offenseTable)
    offensePct <- round(offenseTable/sum(offenseTable)*100)
    offenseLabels<-labels(offenseTable)[[1]]
    offenseLabels<-paste(offenseLabels, offensePct)
    offenseLabels<-paste(offenseLabels, "%",sep=" ")
    
    pie(offenseTable,labels = offenseLabels, col=c("gray48","gray48","gray48","gray48","red4","gray48","gray48","gray48","green4","green4"),
        main=cbind(input$GenOffenseDropD, " Offenses by district"))
    
  })
  
  output$genCrimePercentByDistrict <- renderPlot({
    
    propGenOffense<-as.data.frame.matrix(prop.table(t(table(iowaR$Convicting.Offense.Type, iowaR$Main.Supervising.District)), 1))
    for(i in 1:ncol(propGenOffense)) {
      propGenOffense[,i]<-multAllBy100(propGenOffense[,i])
    }
    
    output$genCrimePercentByDistrict <- renderPlot({
      validate(
        need(input$GenOffenseDropD != "All", "Please select a General Offense Type")
      )
      barplot(t(propGenOffense[input$GenOffenseDropD]), beside=TRUE,
              col=c("gray48","gray48","gray48","gray48","red4","gray48","gray48","gray48","green4","green4"),
              xlab = "Supervising District", ylab = "Percentage of Offense Types in district", main=input$GenOffenseDropD)
    })
    
  })
  
  output$AgeVdistrict <- renderPlot({
    propAge<-as.data.frame.matrix(prop.table(t(table(iowaR$Age.At.Release, iowaR$Main.Supervising.District)), 1))
    for(i in 1:ncol(propAge)) {
      propAge[,i]<-multAllBy100(propAge[,i])
    }
    
    output$AgeVdistrict <- renderPlot({
      validate(
        need(input$AgeDropD != "All", "Please select an Age Group")
      )
      barplot(t(propAge[input$AgeDropD]), beside=TRUE,
              col=c("gray48","gray48","gray48","gray48","red4","gray48","gray48","gray48","green4","green4"),
              xlab = "Supervising District", ylab = "Percentage of Offense Types in district", main=input$AgeDropD)
    })
    
  })
  
  output$AgeVrecidivism <- renderPlot({
    
    if(input$AgeDropD != "All") {
      plot(iowaR$Recidivism...Return.to.Prison[iowaR$Age.At.Release == input$AgeDropD], xlab="Return to Prison?",
           ylab="Number of Inmates", col=c("green3","red3"), main=input$AgeDropD)
    } else {
      plot(iowaR$Recidivism...Return.to.Prison, xlab="Return to Prison?",
           ylab="Number of Inmates", col=c("green3","red3"), main=input$AgeDropD)
    }
  })
  
  

  
  AllOption<-function(colName, inputName) {
    if(inputName == "All") {
      return(colName)
    } else {
      return(inputName)
    }
  }
  
  output$BarAllRecidivism <- renderPlot({
    
    plot(iowaR$Recidivism...Return.to.Prison[iowaR$Age.At.Release == AllOption(iowaR$Age.At.Release, input$AgeDropD) 
                                             & iowaR$Convicting.Offense.Subtype == AllOption(iowaR$Convicting.Offense.Subtype, input$OffenseDropD)
                                             & iowaR$Convicting.Offense.Type == AllOption(iowaR$Convicting.Offense.Type, input$GenOffenseDropD)
                                             & iowaR$Release.Type == AllOption(iowaR$Release.Type, input$ReleaseDropD)], xlab="Return to Prison?",
         ylab="Number of Inmates", col=c("green3","red3"), main=cbind(input$ReleaseDropD, input$OffenseDropD, input$GenOffenseDropD, input$AgeDropD))
  })
  
  output$pieAllRecidivism <- renderPlot ({
    validate(
      need(length(iowaR$Main.Supervising.District[iowaR$Age.At.Release == AllOption(iowaR$Age.At.Release, input$AgeDropD) 
                                                  & iowaR$Convicting.Offense.Subtype == AllOption(iowaR$Convicting.Offense.Subtype, input$OffenseDropD)
                                                  & iowaR$Convicting.Offense.Type == AllOption(iowaR$Convicting.Offense.Type, input$GenOffenseDropD)
                                                  & iowaR$Release.Type == AllOption(iowaR$Release.Type, input$ReleaseDropD)]) > 0, "No Results")
    )
    
  recidTable<-table(iowaR$Recidivism...Return.to.Prison[iowaR$Age.At.Release == AllOption(iowaR$Age.At.Release, input$AgeDropD) 
                                                        & iowaR$Convicting.Offense.Subtype == AllOption(iowaR$Convicting.Offense.Subtype, input$OffenseDropD)
                                                        & iowaR$Convicting.Offense.Type == AllOption(iowaR$Convicting.Offense.Type, input$GenOffenseDropD)
                                                        & iowaR$Release.Type == AllOption(iowaR$Release.Type, input$ReleaseDropD)])
  recidPct <- round(recidTable/sum(recidTable)*100)
  recidLabels<-labels(recidTable)[[1]]
  recidLabels<-paste(recidLabels, recidPct)
  recidLabels<-paste(recidLabels, "%",sep=" ")
  pie(recidTable,labels = recidLabels, col=c("green3","red3"), xlab="Return to Prison?",
      main=cbind(input$ReleaseDropD, input$OffenseDropD, input$GenOffenseDropD, input$AgeDropD))
  })
  
  
  
  output$RecidInEachDistrict <- renderPlot ({
    
    validate(
      need(length(iowaR$Main.Supervising.District[iowaR$Age.At.Release == AllOption(iowaR$Age.At.Release, input$AgeDropD) 
                                                  & iowaR$Convicting.Offense.Subtype == AllOption(iowaR$Convicting.Offense.Subtype, input$OffenseDropD)
                                                  & iowaR$Convicting.Offense.Type == AllOption(iowaR$Convicting.Offense.Type, input$GenOffenseDropD)
                                                  & iowaR$Release.Type == AllOption(iowaR$Release.Type, input$ReleaseDropD)]) > 0, "No Results")
    )
    
    plot(iowaR$Main.Supervising.District[iowaR$Age.At.Release == AllOption(iowaR$Age.At.Release, input$AgeDropD) 
                                         & iowaR$Convicting.Offense.Subtype == AllOption(iowaR$Convicting.Offense.Subtype, input$OffenseDropD)
                                         & iowaR$Convicting.Offense.Type == AllOption(iowaR$Convicting.Offense.Type, input$GenOffenseDropD)
                                         & iowaR$Release.Type == AllOption(iowaR$Release.Type, input$ReleaseDropD)]
         , iowaR$Recidivism...Return.to.Prison[iowaR$Age.At.Release == AllOption(iowaR$Age.At.Release, input$AgeDropD) 
                                                                              & iowaR$Convicting.Offense.Subtype == AllOption(iowaR$Convicting.Offense.Subtype, input$OffenseDropD)
                                                                              & iowaR$Convicting.Offense.Type == AllOption(iowaR$Convicting.Offense.Type, input$GenOffenseDropD)
                                                                              & iowaR$Release.Type == AllOption(iowaR$Release.Type, input$ReleaseDropD)]
         , col=c("green4", "red4"), xlab="district", ylab="return to prison?", main=cbind(input$ReleaseDropD, input$OffenseDropD, input$GenOffenseDropD, input$AgeDropD))
  })
  
  
  
})
