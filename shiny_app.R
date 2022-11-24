

###################################
### Author:  Zhen Lu
### PhD Student
### Date:    2022/11/23
###################################


# git
# usethis::create_github_token()
# gitcreds::gitcreds_set()
# usethis::edit_git_config()
# usethis::use_git()
# usethis::use_github(protocol="https")

# usethis::git_remotes()
# usethis::use_git_remote(name = "origin", url = NULL, overwrite = TRUE)



# install.packages('shinythemes')
suppressMessages(library(shiny))
suppressMessages(library(magrittr))
suppressMessages(library(xgboost))
suppressMessages(library(tidyverse))
suppressMessages(library(shinythemes))



# Fluid page lays out your inputs and outputs and accepts inputs.
ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Machine-learning-based diagnosis for neurosyphilis",
  
  tabPanel('The full models',
  sidebarLayout(
    sidebarPanel('Overall',
                 br(),
                 helpText('Welcome to the Neurosyphilis Diagnosis Shiny app.'),
                 helpText('You can use this app to accurately predict the neurosyphilis outcome in confirmed syphilis patients without HIV infection by six authoritative clinical guidelines globally using laboratory data.'),
                 helpText('This page contains the full version of machine learning models in Scenario 1 and 2.'),
                 width = 2
                 ),
    mainPanel(width = 10,
      fluidRow(
      column(
        width = 3,
        h4("1. Diagnostic Details"),
        selectInput(
          inputId = "cg1",
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
        selectInput(
          inputId = "ds1",
          label = "Please select a dignostic scenario:",
          choices = c('Scenario 1'= 1,
                      'Scenario 2'= 2
          ),
          selected = 1
        ),
        br(),
        
        h4("2. HIV Infection Status"),
        selectInput(
          inputId = "hiv1",
          label = "HIV infection:",
          choices = c('Negative'= 0,
                      'Positive'= 1),
          selected = 0
        ),
        textOutput('c_hiv1'),
        br()),
      
      column(
        width = 3,
        h4("3. Clinical Features"),
        selectInput(
          "s_tppa1", 
          "Serum TPPA titer:",
          choices = c('Negative'= 0,
                      'Positive'= 1),
          selected = 1
        ),
        selectInput(
          "s_igg1", 
          "Serum TP-IgG:",
          choices = c('Negative'= 0,
                      'Positive'= 1),
          selected = 1
        ),
        selectInput(
          "s_trust1", 
          "Serum TRUST:",
          choices = c('Negative'= 0,
                      'Positive'= 1),
          selected = 1
        ),
        numericInput(
          inputId = "c_pro1",
          label = "CSF protein(mg/dL):",
          value = "20",
          min = 0,
          max = 100
        ),
        numericInput(
          inputId = "c_wbc1",
          label = "CSF WBC(/mL):",
          value = "2",
          min = 0,
          max = 100
        ),
        selectInput(
          "c_tppa1", 
          "CSF TPPA titer",
          choices = c('Negative'= 0,
                      'Positive'= 1),
          selected = 0
        ),
        selectInput(
          "c_igg1", 
          "CSF TP-IgG",
          choices = c('Negative'= 0,
                      'Positive'= 1),
          selected = 0
        ),
        selectInput(
          "c_vdrl1", 
          "CSF VDRL",
          choices = c('Negative'= 0,
                      'Positive'= 1),
          selected = 0
        ),
        selectInput(
          "c_trust1", 
          "CSF TRUST titer",
          choices = c('Negative'= 0,
                      'Positive'= 1),
          selected = 0
        ),
        selectInput(
          "syptom1", 
          "Neurological symptoms",
          choices = c('Negative'= 0,
                      'Positive'= 1),
          selected = 0
        )),
      
      column(
        width = 6,
        h3("Prediction and Classification"),
        em("*Risk defined as the probability specifying the likelihood of classification to neurosyphilis for each patient. Diagnosis assessed at the threshold probability of 50%."),
        br(),
        br(),
        h4("Risk Prediction and Final Diagnosis"),
        tableOutput('risk_table1')
      )
    ))
  )
),

tabPanel('The simplified models in Scenario 1',
         sidebarLayout(
           sidebarPanel('Overall',
                        br(),
                        helpText('Welcome to the Neurosyphilis Diagnosis Shiny app.'),
                        helpText('This page contains the simplified version of machine learning models in Scenario 1.'),
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
                           "c_tppa2", 
                           "CSF TPPA titer",
                           choices = c('Negative'= 0,
                                       'Positive'= 1),
                           selected = 0
                         ),
                         selectInput(
                           "syptom2", 
                           "Neurological symptoms",
                           choices = c('Negative'= 0,
                                       'Positive'= 1),
                           selected = 0
                         )),
                       
                       column(
                         width = 6,
                         h3("Prediction and Classification"),
                         em("*Risk defined as the probability specifying the likelihood of classification to neurosyphilis for each patient. Diagnosis assessed at the threshold probability of 50%."),
                         br(),
                         br(),
                         h4("Risk Prediction and Final Diagnosis"),
                         tableOutput('risk_table2')
                       )
                     ))
         )
),

tabPanel('The simplified models in Scenario 2',
         sidebarLayout(
           sidebarPanel('Overall',
                        br(),
                        helpText('Welcome to the Neurosyphilis Diagnosis Shiny app.'),
                        helpText('This page contains the simplified version of machine learning models in Scenario 2.'),
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
                         numericInput(
                           inputId = "c_wbc3",
                           label = "CSF WBC(/mL):",
                           value = "2",
                           min = 0,
                           max = 100
                         ),
                         selectInput(
                           "c_vdrl3", 
                           "CSF VDRL / CSF TRUST / CSF RPR (any one of the three)",
                           choices = c('Negative'= 0,
                                       'Positive'= 1),
                           selected = 0
                         ),
                         selectInput(
                           "syptom3", 
                           "Neurological symptoms",
                           choices = c('Negative'= 0,
                                       'Positive'= 1),
                           selected = 0
                         )),
                       
                       column(
                         width = 6,
                         h3("Prediction and Classification"),
                         em("*Risk defined as the probability specifying the likelihood of classification to neurosyphilis for each patient. Diagnosis assessed at the threshold probability of 50%."),
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



# calculates your outputs and any other calculations needed for outputs
server <- function(input, output){
  
  output$c_hiv1 <- renderText({
    ccsValid1= input$hiv1 %>% as.numeric() %>% as.logical()
    result1 <- ifelse(!ccsValid1, "Please enter the following clinical features",
                     paste0("We do not support neurosyphilis diagnosis in HIV-infected syphilis patients nowadays."))
    if(ccsValid1) print(result1)
  })
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
  
  output$risk_table1 <- renderTable({
    ccsValid1= input$hiv1 %>% as.numeric() %>% as.logical()
    
    if(as.numeric(input$ds1) == 1){
      dsLabel1= c( 'outData1_op1_China_2020', 'outData1_op1_Euro_2020_08',
                  'outData1_op1_NT_Aust_2022', 'outData1_op1_Uptodate_2020',
                  'outData1_op1_US_CDC_2018', 'outData1_op1_US_2021')
    }else{
      dsLabel1= c( 'outData1_op2_China_2020', 'outData1_op2_Euro_2020_08',
                  'outData1_op2_NT_Aust_2022', 'outData1_op2_Uptodate_2020',
                  'outData1_op2_US_CDC_2018', 'outData1_op2_US_2021')
    }
    r1= 1:6
    r1[as.numeric(input$cg1)]= r1[as.numeric(input$cg1)]-1000
    dsLabel1= dsLabel1[order(r1)]
    n_meds1= length(dsLabel1)
    
    pp1= data.frame(input$s_trust1, input$s_tppa1, input$s_igg1, input$c_igg1, input$c_pro1, 
                   input$c_wbc1, input$c_trust1, input$c_tppa1, input$c_vdrl1, input$syptom1)
    names(pp1)[1:10]= c('Serum TRUST', 'Serum TPPA titer',
                       'Serum TP-IgG', 'CSF TP-IgG',
                       'CSF protein', 'CSF WBC',
                       'CSF TRUST', 'CSF TPPA titer', 'CSF VDRL',
                       'Neurological symptoms'
    )
    
    ppData_tmp1= pp1 %>% apply(.,2,as.numeric) %>% as.data.frame() %>% t() %>%  as.matrix()
    dMtrxpp_tmp1 = xgb.DMatrix(ppData_tmp1)
    
    if(!ccsValid1){
      for (i in 1:n_meds1) {
        load(file = sprintf("./Models/TheFullModels/ROC_curve_%s.Rdata", dsLabel1[i]))
        risk_score1 <- predict(model_my_xgb, dMtrxpp_tmp1) * 100
        risk_score1= cbind(str_sub(dsLabel1[i], start = 14), as.data.frame(risk_score1))
        if(i== 1){
          risk1= risk_score1
        }else{
          risk1= rbind(risk1, risk_score1)
        }
      }
      
      colnames(risk1) <- c("cx_abbr", "Risk (%)")
      dict1 <- data.frame(matrix(c(c('China 2020',"China_2020"),
                                  c('Europe 2020',"Euro_2020_08"),
                                  c('NT Australia 2022',"NT_Aust_2022"),
                                  c('UpToDate 2020',"Uptodate_2020"),
                                  c('US CDC 2018',"US_CDC_2018"),
                                  c('US 2021',"US_2021")), ncol = 2, byrow = T))
      colnames(dict1) <- c("Clinical guidelines", "cx_abbr")
      dict1$sort= 1:6
      result1 <- merge(dict1, risk1)
      result1 %<>% arrange(sort) %>% select(-sort)
      result1 <- result1[, 2:3]
      result1= result1[order(r1),]
      result1[-1,] <- result1[-1,][order(-rank(result1[-1,"Risk (%)"])),]
      result1$Diagnosis <- ifelse(result1[["Risk (%)"]] >= 50, "Neurosyphilis", "Non-neurosyphilis")
    } else {
      result1 <- "We do not support neurosyphilis diagnosis in HIV-infected syphilis patients nowadays."
    }
    return(result1)
  })
  
  output$risk_table2 <- renderTable({
    ccsValid2= input$hiv2 %>% as.numeric() %>% as.logical()
    
    dsLabel2= c( 'outData1_op1_China_2020', 'outData1_op1_Euro_2020_08',
                 'outData1_op1_NT_Aust_2022', 'outData1_op1_Uptodate_2020',
                 'outData1_op1_US_CDC_2018', 'outData1_op1_US_2021')
    r2= 1:6
    r2[as.numeric(input$cg2)]= r2[as.numeric(input$cg2)]-2000
    dsLabel2= dsLabel2[order(r2)]
    n_meds2= length(dsLabel2)
    
    pp2= data.frame(input$c_wbc2, input$c_pro2, input$syptom2, input$c_tppa2)
    names(pp2)[1:4]= c('CSF WBC', 'CSF protein', 'Neurological symptoms',
                        'CSF TPPA titer')
    
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
      dict2$sort= 1:6
      result2 <- merge(dict2, risk2)
      result2 %<>% arrange(sort) %>% select(-sort)
      result2 <- result2[, 2:3]
      result2= result2[order(r2),]
      result2[2:6,] <- result2[2:6,][order(-rank(result2[2:6,"Risk (%)"])),]
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
    r3= 1:6
    r3[as.numeric(input$cg3)]= r3[as.numeric(input$cg3)]-3000
    dsLabel3= dsLabel3[order(r3)]
    n_meds3= length(dsLabel3)
    
    pp3= data.frame(input$syptom3, input$c_vdrl3, input$c_pro3, 
                    input$c_wbc3)
    names(pp3)[1:4]= c(
                       'Neurological symptoms', 'CSF VDRL', 'CSF protein', 'CSF WBC'
                        
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
      dict3$sort= 1:6
      result3 <- merge(dict3, risk3)
      result3 %<>% arrange(sort) %>% select(-sort)
      result3 <- result3[, 2:3]
      result3= result3[order(r3),]
      result3[2:6,] <- result3[2:6,][order(-rank(result3[2:6,"Risk (%)"])),]
      result3$Diagnosis <- ifelse(result3[["Risk (%)"]] >= 50, "Neurosyphilis", "Non-neurosyphilis")
    } else {
      result3 <- "We do not support neurosyphilis diagnosis in HIV-infected syphilis patients nowadays."
    }
    return(result3)
  })
  
}

shinyApp(ui = ui, server = server)

