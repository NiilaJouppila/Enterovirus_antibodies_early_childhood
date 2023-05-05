#set the working directory to a folder that contains this script and the accompanying files
list.of.packages <- c("ggplot2","reshape2","shiny")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(dplyr)
library(ggplot2)
library(reshape2)
library(shiny)
##################Load data###############
EVAs<-as.data.frame(read.csv2("PSEUDO_ID_EVAs.csv"))
EVBs<-as.data.frame(read.csv2("PSEUDO_ID_EVBs.csv"))
EVCs<-as.data.frame(read.csv2("PSEUDO_ID_EVCs.csv"))
EVDs<-as.data.frame(read.csv2("PSEUDO_ID_EVDs.csv"))
RVAs<-as.data.frame(read.csv2("PSEUDO_ID_RVAs.csv"))
RVBs<-as.data.frame(read.csv2("PSEUDO_ID_RVBs.csv"))
RVCs<-as.data.frame(read.csv2("PSEUDO_ID_RVCs.csv"))
###Data preparation###
EVAs<-EVAs%>%
  select(Serotype..VP1.,Pseudo_ID,days_since_detection:X3C,VISIT)%>%
  melt(id.vars=c("Pseudo_ID", "days_since_detection","Serotype..VP1.","VISIT"))
EVBs<-EVBs%>%
  select(Serotype..VP1.,Pseudo_ID,days_since_detection:X3C,VISIT)%>%
  melt(id.vars=c("Pseudo_ID", "days_since_detection","Serotype..VP1.","VISIT"))
EVCs<-EVCs%>%
  select(Serotype..VP1.,Pseudo_ID,days_since_detection:X3C,VISIT)%>%
  melt(id.vars=c("Pseudo_ID", "days_since_detection","Serotype..VP1.","VISIT"))
EVDs<-EVDs%>%
  select(Serotype..VP1.,Pseudo_ID,days_since_detection:X3C,VISIT)%>%
  melt(id.vars=c("Pseudo_ID", "days_since_detection","Serotype..VP1.","VISIT"))
RVAs<-RVAs%>%
  select(Serotype..VP1.,Pseudo_ID,days_since_detection:X3C,VISIT)%>%
  melt(id.vars=c("Pseudo_ID", "days_since_detection","Serotype..VP1.","VISIT"))
RVBs<-RVBs%>%
  select(Serotype..VP1.,Pseudo_ID,days_since_detection:X3C,VISIT)%>%
  melt(id.vars=c("Pseudo_ID", "days_since_detection","Serotype..VP1.","VISIT"))
RVCs<-RVCs%>%
  select(Serotype..VP1.,Pseudo_ID,days_since_detection:X3C,VISIT)%>%
  melt(id.vars=c("Pseudo_ID", "days_since_detection","Serotype..VP1.","VISIT"))
###Application interface and server###
ui <- fluidPage(
  # Application title
  titlePanel("Enterovirus VP1 and protease antibody signals by infection group"),
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(selectInput("datas",
                             "PCR positivity:",
                             choices=c("EVAs", "EVBs","EVCs","EVDs","RVAs","RVBs","RVCs")),
                 checkboxGroupInput("antigens","Choose antigens",choices=c("CAV4","CVB1","Echo30","Polio","EVD68","RVA","RVB","RVC","X2A","X3C")),
                 sliderInput("ageinmonths","Age in months",min=0,max=36,value=c(0,36),step=3),
                 sliderInput("dpi","Days to infection:", min=-300, max=300, value=c(-300,300),step=10),
                 submitButton("Update View", icon("refresh")),width=2),
    mainPanel(plotOutput("test_plot"),
              textOutput("slider"),width=10)
  )
)    
server <- function(input, output) {
  base <- reactive ({get(input$datas) })
  output$test_plot<-renderPlot({
    mydata<-base()
    mydata%>%distinct(value, .keep_all = TRUE)%>%filter((variable %in% input$antigens)&(VISIT>=input$ageinmonths[1]&VISIT<=input$ageinmonths[2]))%>%ggplot(aes(x=days_since_detection, y=value,label=VISIT,colour=variable))+geom_point()+geom_text(aes(label=VISIT),hjust=0, vjust=2)+
      geom_line(alpha=0.6)+annotate("rect", xmin = input$dpi[1], xmax = input$dpi[2], ymin = 0, ymax = 1000000, alpha = .2, fill="grey64")+facet_wrap(vars(Pseudo_ID))+theme_minimal()+scale_y_log10()
  })
}
shinyApp(ui = ui, server = server)