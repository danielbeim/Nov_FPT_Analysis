library(tidyverse)
data <- readxl::read_xlsx("Sep-Nov_SC_Data.xlsx")
IDs <- readxl::read_xlsx("NetSuite_NS_IDs.xlsx")
Downfunnel <- readxl::read_xlsx("Downfunnel Performance - LI - 9.1.23-12.4.24.xlsx")
IDs<- IDs[,1:2]

#mergeIDs
data <- merge(x = data, y = IDs, by.x = "Campaign Name", by.y = "LinkedIn Campaign Name", all.x = TRUE)
#reorder columns 
data <- data[-c(31,52),]
data <- data[,c(1,9,2:8)]

#group Downfunnel by MQL data for select months
Downfunnel %>% filter(between(`MQL Date (for cohort)`, as.Date('2024-09-01'), as.Date('2024-11-30'))) -> Downfunnel

Downfunnel <- Downfunnel %>%
  group_by(Campaign) %>% 
  filter(`Customer Metric Name` == "MQL") %>%
  summarise(MQL = n())

#Merge MQLs to data
data <- merge(x = data, y = Downfunnel, by.x = "NetSuite CID Form", by.y = "Campaign", all.x = TRUE)
data$MQL[is.na(data$MQL)] <- 0 #sets missing values as zero MQLs

data <- data[,c(2,1,3:10)] #reorder

#Adds cost per MQL to the data
data <- data %>% mutate(CPMQL = `Total Spent` / MQL)
data$CPMQL <- round(data$CPMQL, 2)
library(formattable)
data$CPMQL <- currency(data$CPMQL)

#Identifies and filters for FPTs
data$Type <- ifelse(grepl('^FPT', data$`Campaign Name`), 'FPT', 'WP')
data <- data %>% filter(Type == "FPT")

#calculates benchmark averages
avgCPMQL <- sum(data$`Total Spent`) / sum(data$MQL)
avgMQL <- mean(data$MQL)

#assign cohorts based on performance above/below average benchmarks
library(dplyr)
data$cohort <- case_when(
  data$MQL > avgMQL & data$CPMQL > avgCPMQL ~ "Cohort A",
  data$MQL > avgMQL & data$CPMQL < avgCPMQL ~ "Cohort B",
  data$MQL < avgMQL & data$CPMQL > avgCPMQL ~ "Cohort C",
  TRUE ~ "Cohort D"
)



install.packages("plotly")
library(plotly)
# Create a scatter plot


#AXIS PARAMATERS
vline <- function(x = 0, color = "black") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    text = "AVG CPMQL",
    line = list(color = color, opacity = 0.5)
  )
}

hline <- function(y = 0, color = "black", text = "AVG MQL") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    text = "AVG MQL",
    line = list(color = color, text = "AVG MQL")
  )
}


library(htmlwidgets)
library(htmltools)

#SC PLOT
plot <- plot_ly(data, x = ~CPMQL, y = ~MQL, type = 'scatter', mode = 'markers',
                color = ~cohort,
                text = ~paste("Campaign: ", `Campaign Name`,
                              "\n", "CP MQL: ", data$CPMQL, "\n", "MQL:", data$MQL),
                hoverinfo = "text",
                colors = c("Green", "Red", "Blue")) %>%
  
  layout(
    xaxis = list(zeroline = FALSE),
    yaxis = list(zeroline = FALSE),
    shapes = list(vline(avgCPMQL), hline(avgMQL)), 
    title = "September - November Sponsored Content FPT Performance",
    annotations = list(
      list(x = avgCPMQL + 75, y = max(data$MQL),
           text = "Average CP MQL: $1,661.72",
           showarrow = FALSE,
           textangle = 90),
      list(x = 5000, y = avgMQL + 0.5,
           text = "Average MQL: 2.5",
           showarrow = FALSE)
    )
  )

plot
output_file1 <- "/Users/danielbeim/Downloads/NS_FPT_Sep-Nov/SCPlot.html"

# Save the Plotly plot as an HTML file
saveWidget(plot, file = output_file1, selfcontained = TRUE)





#Repeat for IM
IMdata <- readxl::read_xlsx("May-July IM data.xlsx")
IDs <- readxl::read_xlsx("NetSuite_NS_IDs.xlsx")
Downfunnel <- readxl::read_xlsx("Downfunnel Performance - LI - 9.1.23 to 8.5.24.xlsx")
IDs<- IDs[,1:2]

#mergeIDs
IMdata <- merge(x = IMdata, y = IDs, by.x = "Campaign Name", by.y = "LinkedIn Campaign Name", all.x = TRUE)
#reorder columns 
IMdata <- IMdata[,c(1,9,2:8)]

#group Downfunnel by MQL data for select months
Downfunnel %>% filter(between(`MQL Date (for cohort)`, as.Date('2024-05-01'), as.Date('2024-07-31'))) -> Downfunnel

Downfunnel <- Downfunnel %>%
  group_by(Campaign) %>% 
  filter(`Customer Metric Name` == "MQL") %>%
  summarise(MQL = n())

#Merge MQLs to data
IMdata <- merge(x = IMdata, y = Downfunnel, by.x = "NetSuite CID Form", by.y = "Campaign", all.x = TRUE)
IMdata$MQL[is.na(IMdata$MQL)] <- 0 #sets missing values as zero MQLs

IMdata <- IMdata[,c(2,1,3:10)] #reorder

#Adds cost per MQL to the data
IMdata <- IMdata %>% mutate(CPMQL = `Total Spent` / MQL)
IMdata$CPMQL <- round(IMdata$CPMQL, 2)

#Identifies and filters for FPTs
IMdata$Type <- ifelse(grepl('^FPT', IMdata$`Campaign Name`), 'FPT', 'WP')
IMdata <- IMdata %>% filter(Type == "FPT")

#calculates benchmark averages
avgCPMQL <- sum(IMdata$`Total Spent`) / sum(IMdata$MQL)
avgMQL <- mean(IMdata$MQL)

#creates cohorts based on whether campaign is above or below benchmarks
library(dplyr)
IMdata$cohort <- case_when(
  IMdata$MQL > avgMQL & IMdata$CPMQL > avgCPMQL ~ "Cohort A",
  IMdata$MQL > avgMQL & IMdata$CPMQL < avgCPMQL ~ "Cohort B",
  IMdata$MQL < avgMQL & IMdata$CPMQL > avgCPMQL ~ "Cohort C",
  TRUE ~ "Cohort D"
)

#IM PLOT
plot2 <- plot_ly(IMdata, x = ~CPMQL, y = ~MQL, type = 'scatter', mode = 'markers',
                color = ~cohort,
                text = ~paste("Campaign: ", `Campaign Name`,
                              "\n", "CP MQL:", IMdata$CPMQL, "\n", "MQL:", IMdata$MQL),
                hoverinfo = "text",
                colors = c("Orange", "Green", "Red", "Blue")) %>%
  
  layout(
    xaxis = list(zeroline = FALSE),
    yaxis = list(zeroline = FALSE),
    shapes = list(vline(avgCPMQL), hline(avgMQL)), 
    title = "May - July InMail FPT Performance",
    annotations = list(
      list(x = avgCPMQL + 75, y = max(IMdata$MQL),
           text = "Average CP MQL: $1,278",
           showarrow = FALSE,
           textangle = 90),
      list(x = 5000, y = avgMQL + 0.5,
           text = "Average MQL: 5.6",
           showarrow = FALSE)
    )
  )

plot2
output_file2 <- "/Users/danielbeim/Downloads/NS_FPT_July_Data/IMPlot.html"

# Save the Plotly plot as an HTML file
saveWidget(plot2, file = output_file2, selfcontained = TRUE)






