#####################################
## 02/2019 Justin Rajendra
## app to read from RAD8 pulse ox
## UI V5 small

#########################################################################
header <- dashboardHeader(title=paste("Julian Sats"),disable=TRUE)
sidebar <- dashboardSidebar(width=275,disable=TRUE
                            
)   ## end sidebar

#########################################################################
body <-  dashboardBody(
  shinyDashboardThemes(theme="grey_dark"),
  fluidRow(
    column(width=8,
           valueBox(
             htmlOutput("dateTimeOutput"),
             subtitle=tags$sp("Date and Time",
                              style="font-size:40px;color:grey;"),
             width=12,icon=icon("clock"),color="navy"))
    
  ),
  fluidRow(
    column(width=4,valueBoxOutput("alarmOutput",width=12))
  ),
  
  fluidRow(
    column(width=4,
           valueBox(
             htmlOutput("spo2Output"),
             subtitle=tags$sp("spO2 (%)",style="font-size:40px;color:grey;"),
             width=12,icon=icon("vial"),color="navy"))
  ),
  
  fluidRow(
    column(width=4,
           valueBox(
             htmlOutput("bpmOutput"),
             subtitle=tags$sp("HR (bpm)",style="font-size:40px;color:grey;"),
             width=12,icon=icon("heart"),color="navy"))
  ),
  fluidRow(
    column(width=4,
           valueBox(
             color="navy",width=12,icon=icon("bell"),
             radioButtons("mute_warning",
                          tags$sp("Stop 93% warning bell?",style="color:grey;"),
                          c("Play","Stop"),
                          selected="Play",inline=TRUE),
             htmlOutput("warning_sound")
           )
    )
  ),
  fluidRow(
    column(width=4,
           box(title=tags$sp("Cuff",style="font-size:40px;color:grey;"),
               status="primary",
               actionButton("cuffDown","Down",icon=icon("arrow-circle-down")),
               htmlOutput("cuffDownOutput"),
               br(),br(),
               actionButton("cuffUp","Up",icon=icon("arrow-circle-up")),
               htmlOutput("cuffUpOutput"),
               br()
           )
    )
  )
)   ## end dashboard body

## run it
dashboardPage(header, sidebar, body)



