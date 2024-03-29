


# install.packages(c('shiny', 'magrittr', 'xgboost', 'tidyverse', 'shinythemes'))
suppressMessages(library(shiny))
suppressMessages(library(magrittr))
suppressMessages(library(xgboost))
suppressMessages(library(tidyverse))
suppressMessages(library(shinythemes))



ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Machine-learning-based diagnosis for neurosyphilis",


tabPanel('Scenario 1',
         sidebarLayout(
           sidebarPanel('Overall',
                        br(),
                        helpText('Welcome to the Shiny App for Neurosyphilis Diagnosis.'),
                        helpText('You can use this to accurately predict the risk of developing a neurosyphilis outcome in syphilis patients', 
                                 span(strong(em('without HIV infection')), style="color:red"),
                                 '.'),
                        helpText('This page contains the version of models in Scenario 1. Scenario 1 encoded uncomplicated syphilis as non-neurosyphilis while others (verified, probable, and possible) as neurosyphilis.'),
                        helpText('Note: Among the six guidelines, only the UpToDate 2020 guideline takes asymptomatic neurosyphilis into consideration.'),
                        width = 2
           ),
           mainPanel(width = 10,
                     fluidRow(
                       column(
                         width = 3,
                         h4("1. Diagnostic Details"),
                         selectInput(
                           inputId = "cg2",
                           label = "Please select a clinical guideline:",
                           choices = c('China 2020'= 1,
                                       'Europe 2020'= 2,
                                       'NT Australia 2022'= 3,
                                       'UpToDate 2020'= 4,
                                       'US CDC 2018'= 5,
                                       'US 2021'= 6
                           ),
                           selected = 1
                         ),
                         br(),

                         h4("2. HIV Infection Status"),
                         selectInput(
                           inputId = "hiv2",
                           label = "HIV infection:",
                           choices = c('Negative'= 0,
                                       'Positive'= 1),
                           selected = 0
                         ),
                         textOutput('c_hiv2'),
                         br()),

                       column(
                         width = 3,
                         h4("3. Clinical Features"),
                         numericInput(
                           inputId = "c_pro2",
                           label = "CSF protein(mg/dL):",
                           value = "20",
                           min = 0,
                           max = 100
                         ),
                         numericInput(
                           inputId = "c_wbc2",
                           label = "CSF WBC(/mL):",
                           value = "2",
                           min = 0,
                           max = 100
                         ),

                         selectInput(
                           "syptom2",
                           "Neurological symptoms",
                           choices = c('Absent'= 0,
                                       'Present'= 1),
                           selected = 0
                         )),

                       column(
                         width = 6,
                         h3("Prediction and Classification"),
                         em("*Risk defined as the probability specifying the likelihood of classification to neurosyphilis for each syphilis patient. Diagnosis assessed at the threshold probability of 50%."),
                         br(),
                         br(),
                         h4("Risk Prediction and Final Diagnosis"),
                         tableOutput('risk_table2')
                       )
                     ))
         )
),

tabPanel('Scenario 2',
         sidebarLayout(
           sidebarPanel('Overall',
                        br(),
                        helpText('Welcome to the Shiny App for Neurosyphilis Diagnosis.'),
                        helpText('You can use this to accurately predict the risk of developing a neurosyphilis outcome in syphilis patients', 
                                 span(strong(em('without HIV infection')), style="color:red"),
                                 '.'),
                        helpText('This page contains the version of models in Scenario 2. Scenario 2 encoded verified as neurosyphilis while others (probable, possible, and non-neurosyphilis) as non-neurosyphilis.'),
                        helpText('Note: Among the six guidelines, only the UpToDate 2020 guideline takes asymptomatic neurosyphilis into consideration.'),
                        width = 2
           ),
           mainPanel(width = 10,
                     fluidRow(
                       column(
                         width = 3,
                         h4("1. Diagnostic Details"),
                         selectInput(
                           inputId = "cg3",
                           label = "Please select a clinical guideline:",
                           choices = c('China 2020'= 1,
                                       'Europe 2020'= 2,
                                       'NT Australia 2022'= 3,
                                       'UpToDate 2020'= 4,
                                       'US CDC 2018'= 5,
                                       'US 2021'= 6
                           ),
                           selected = 1
                         ),
                         br(),
                         
                         h4("2. HIV Infection Status"),
                         selectInput(
                           inputId = "hiv3",
                           label = "HIV infection:",
                           choices = c('Negative'= 0,
                                       'Positive'= 1),
                           selected = 0
                         ),
                         textOutput('c_hiv3'),
                         br()),
                       
                       column(
                         width = 3,
                         h4("3. Clinical Features"),
                         numericInput(
                           inputId = "c_pro3",
                           label = "CSF protein(mg/dL):",
                           value = "20",
                           min = 0,
                           max = 100
                         ),

                         selectInput(
                           "c_vdrl3", 
                           "CSF VDRL/RPR",
                           choices = c('Negative'= 0,
                                       'Positive'= 1),
                           selected = 0
                         ),
                         selectInput(
                           "syptom3", 
                           "Neurological symptoms",
                           choices = c('Absent'= 0,
                                       'Present'= 1),
                           selected = 0
                         )),
                       
                       column(
                         width = 6,
                         h3("Prediction and Classification"),
                         em("*Risk defined as the probability specifying the likelihood of classification to neurosyphilis for each syphilis patient. Diagnosis assessed at the threshold probability of 50%."),
                         br(),
                         br(),
                         h4("Risk Prediction and Final Diagnosis"),
                         tableOutput('risk_table3')
                       )
                     ))
         )
)
)
)




server <- function(input, output){
  
  output$c_hiv2 <- renderText({
    ccsValid2= input$hiv2 %>% as.numeric() %>% as.logical()
    result2 <- ifelse(!ccsValid2, "Please enter the following clinical features",
                     paste0("We do not support neurosyphilis diagnosis in HIV-infected syphilis patients nowadays."))
    if(ccsValid2) print(result2)
  })
  output$c_hiv3 <- renderText({
    ccsValid3= input$hiv3 %>% as.numeric() %>% as.logical()
    result3 <- ifelse(!ccsValid3, "Please enter the following clinical features",
                     paste0("We do not support neurosyphilis diagnosis in HIV-infected syphilis patients nowadays."))
    if(ccsValid3) print(result3)
  })
  
  
  output$risk_table2 <- renderTable({
    ccsValid2= input$hiv2 %>% as.numeric() %>% as.logical()
    
    dsLabel2= c( 'outData1_op1_China_2020', 'outData1_op1_Euro_2020_08',
                 'outData1_op1_NT_Aust_2022', 'outData1_op1_Uptodate_2020',
                 'outData1_op1_US_CDC_2018', 'outData1_op1_US_2021')
    dsLabel2= dsLabel2[as.numeric(input$cg2)]

    n_meds2= length(dsLabel2)
    
    pp2= data.frame(input$syptom2, input$c_pro2, input$c_wbc2)
    names(pp2)[1:3]= c('Neurological symptoms', 'CSF protein', 'CSF WBC')
    
    ppData_tmp2= pp2 %>% apply(.,2,as.numeric) %>% as.data.frame() %>% t() %>%  as.matrix()
    dMtrxpp_tmp2 = xgb.DMatrix(ppData_tmp2)
    
    if(!ccsValid2){
      for (i in 1:n_meds2) {
        load(file = sprintf("./Models/TheSimplifiedModelsinScenario1/ROC_curve_%s.Rdata", dsLabel2[i]))
        risk_score2 <- predict(model_my_xgb, dMtrxpp_tmp2) * 100
        risk_score2= cbind(str_sub(dsLabel2[i], start = 14), as.data.frame(risk_score2))
        if(i== 1){
          risk2= risk_score2
        }else{
          risk2= rbind(risk2, risk_score2)
        }
      }
      
      colnames(risk2) <- c("cx_abbr", "Risk (%)")
      dict2 <- data.frame(matrix(c(c('China 2020',"China_2020"),
                                   c('Europe 2020',"Euro_2020_08"),
                                   c('NT Australia 2022',"NT_Aust_2022"),
                                   c('UpToDate 2020',"Uptodate_2020"),
                                   c('US CDC 2018',"US_CDC_2018"),
                                   c('US 2021',"US_2021")), ncol = 2, byrow = T))
      
      colnames(dict2) <- c("Clinical guidelines", "cx_abbr")

      result2 <- merge(dict2, risk2)

      result2 <- result2[, 2:3]

      result2 <- result2[order(-rank(result2[["Risk (%)"]])),]
      result2$Diagnosis <- ifelse(result2[["Risk (%)"]] >= 50, "Neurosyphilis", "Non-neurosyphilis")
    } else {
      result2 <- "We do not support neurosyphilis diagnosis in HIV-infected syphilis patients nowadays."
    }
    return(result2)
  })
  
  output$risk_table3 <- renderTable({
    ccsValid3= input$hiv3 %>% as.numeric() %>% as.logical()
    
    dsLabel3= c( 'outData1_op2_China_2020', 'outData1_op2_Euro_2020_08',
                 'outData1_op2_NT_Aust_2022', 'outData1_op2_Uptodate_2020',
                 'outData1_op2_US_CDC_2018', 'outData1_op2_US_2021')
    dsLabel3= dsLabel3[as.numeric(input$cg3)]

    n_meds3= length(dsLabel3)
    
    pp3= data.frame(input$syptom3, input$c_vdrl3, input$c_pro3)
    names(pp3)[1:3]= c(
                       'Neurological symptoms', 'CSF VDRL', 'CSF protein'
                        
    )
    
    ppData_tmp3= pp3 %>% apply(.,2,as.numeric) %>% as.data.frame() %>% t() %>%  as.matrix()
    dMtrxpp_tmp3 = xgb.DMatrix(ppData_tmp3)
    
    if(!ccsValid3){
      for (i in 1:n_meds3) {
        load(file = sprintf("./Models/TheSimplifiedModelsinScenario2/ROC_curve_%s.Rdata", dsLabel3[i]))

        risk_score3 <- predict(model_my_xgb, dMtrxpp_tmp3) * 100
        risk_score3= cbind(str_sub(dsLabel3[i], start = 14), as.data.frame(risk_score3))
        if(i== 1){
          risk3= risk_score3
        }else{
          risk3= rbind(risk3, risk_score3)
        }
      }
      
      colnames(risk3) <- c("cx_abbr", "Risk (%)")
      dict3 <- data.frame(matrix(c(c('China 2020',"China_2020"),
                                   c('Europe 2020',"Euro_2020_08"),
                                   c('NT Australia 2022',"NT_Aust_2022"),
                                   c('UpToDate 2020',"Uptodate_2020"),
                                   c('US CDC 2018',"US_CDC_2018"),
                                   c('US 2021',"US_2021")), ncol = 2, byrow = T))
      
      colnames(dict3) <- c("Clinical guidelines", "cx_abbr")

      result3 <- merge(dict3, risk3)

      result3 <- result3[, 2:3]

      result3 <- result3[order(-rank(result3[["Risk (%)"]])),]
      result3$Diagnosis <- ifelse(result3[["Risk (%)"]] >= 50, "Neurosyphilis", "Non-neurosyphilis")
    } else {
      result3 <- "We do not support neurosyphilis diagnosis in HIV-infected syphilis patients nowadays."
    }
    return(result3)
  })
  
}

shinyApp(ui = ui, server = server)

