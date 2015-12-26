library(shiny)
library(ggplot2)
data(msleep)
sleepdifference <- function(animal1, animal2) abs(animal1 - animal2)

shinyServer(
        function(input, output) {
                output$mammal1 <- renderText({paste("Your first selection, the", input$mammal1, "sleeps", msleep$sleep_total[msleep$name == input$mammal1], "average hours per day.")})
                output$mammal2 <- renderText({paste("Your second selection, the", input$mammal2, "sleeps", msleep$sleep_total[msleep$name == input$mammal2], "average hours per day.")})
                output$sleeptime2 <- renderPrint({msleep$sleep_total[msleep$name == input$mammal2]})
                output$difference <- renderText({paste("The total sleep difference between", input$mammal1, "and", input$mammal2, "is", sleepdifference(msleep$sleep_total[msleep$name == input$mammal1], msleep$sleep_total[msleep$name == input$mammal2]), "hours.")})
                output$chartdiscription <- renderText({paste(input$mammal1, "is the green line,", input$mammal2, "is blue.")})
                output$distribution <- renderPlot({
                        hist(msleep$sleep_total, breaks = 15, col = "grey", xlim = c(0,20), xlab = "Daily Sleep", main = "Sleep Distribution of Mammals")
                        abline(v = msleep$sleep_total[msleep$name == input$mammal1], col = "darkgreen", lwd = 6)
                        abline(v = msleep$sleep_total[msleep$name == input$mammal2], col = "darkblue", lwd = 6)
                })
                output$documentation <- renderText("The purpose of this tool is to help users understand the sleep characteristics of mammals. Users can see the difference between two specific mammals and compare both to the overall mammal population. This is based on the dataset in R called msleep. The data consists of information on 83 mammals. The tool provides two dropdown input selectors of all 83 mammals in the data. From there the tool provides the average sleep total sleep time in a day for both of the selected animals. Users can then also see the difference in daily sleep between the two selected mammals. Finally, it calculates the position of those animal's sleep against a distribution of all 83 mammals."
                                                    )
     ##           output$documentation <- renderUI({
     ##                   str1 <- paste("You have selected", input$var)
     ##                   str2 <- paste("You have chosen a range that goes from",
     ##                                 input$range[1], "to", input$range[2])
     ##                   HTML(paste(str1, str2, sep = '<br/>'))
                        
     ##           })
        }
)