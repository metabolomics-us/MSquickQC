#essential columns (normalized data) are: sampleType, time, and label
#essential columns (annotations and MSI) : Class, MSMS match, mzrt match, 
# Similar MSMS match, Similar mzrt match

options(shiny.maxRequestSize = 50 * 1024^2)

# Load libraries

library(shiny)
library(readxl)
library(DT)
library(shinyBS)

# Define UI

#### UI - Instructions ####

ui_instructions <- sidebarLayout(
  
sidebarPanel(

  h4("Getting Started"),
  
  br("Please consult the ",
  tags$a("User Instructions",
         href = "Instructions.docx"),
  "for detailed data formatting requirements and
  troubleshooting tips."),
  
  br("Download an",
  tags$a("Example data",
         href = "example_file.xlsx"),
  "set.")
),
  
 mainPanel(
                                   
   h5(strong("Upload Instructions:")
      ),
   
   br("- Data file ", strong("must",
       style = "color:red") ,
      " be uploaded as a .xlsx file."),
   
   br("- The first sheet in the workbook 
      must contain the following
      columns in order:", 
      strong("sampleType, time, label. ",
      style = "color:red"),
      "These columns are case-sensitive and 
      correspond to the sample type
      (Sample or QC1, QC2, etc.), the injection order, 
      and each sample name."),
   
      br("- Please include a second sheet in the
         same file for assignment of MSI levels; leave blank if 
         this information is not available (detailed formatting 
         instructions are provided in the ",
         strong("User Instructions ",
         style = "color:red"),
         "file at the top of this page.)."),
   
                                   br()  
         )
  )

#### UI - Data Upload ####

ui_upload <- sidebarLayout(
  
  sidebarPanel(
    
    h4("Data Sheet Upload:"),
                 
    fileInput('file1', 'Upload normalized data sheet',
                           accept = c(".xlsx")
                 ),
                 
    radioButtons('msi', 'Would you like annotation 
                            and MSI level information 
                            included in the report? 
                            (must have accompanying Excel sheet)',
                              c('Yes' = 'y',
                                'No' = 'n'))
        ),
    
    
  mainPanel(
        
    tabsetPanel(
            
      tabPanel("Normalized Data",
                     DT::dataTableOutput('contents1', 
                                         width = "100%",
                                         height = "auto")),
            
      tabPanel("Annotation/MSI Information",
                     DT::dataTableOutput('contents2', 
                                         width = "100%",
                                         height = "auto"))
                )
            )
    )

#### UI - Study Information ####

ui_study <- sidebarLayout(
    sidebarPanel(
        h4("Study Information"),
        h5(strong("(*) ",
                  style = "color:red"), "Indicates a required field"),
        
        textInput('studyname', 'Study Name', 
                  value = 'ex. mx626980 Mouse'),
        textInput('name', 'Prepared By', 
                  value = 'Your Name'),
        textInput('chro', 'Chromatography',
                  value = 'ex. LC-MS/MS'),
        textInput('inst', 'Instrument',
                  value = 'ex. Thermo Q-Exactive HF'),
        textInput('plat', 'Analysis Platform and Ionization', 
                  value = 'ex. CSH POS'),
        textInput('batch', 'Number of Batches', 
                  value = 'ex 1-10'),
        textInput('date', 'Analysis Date', 
                  value = 'ex. November 2020')
        ),
    mainPanel(
        
    )
)



ui_dpqc <- sidebarLayout(
    sidebarPanel(
        h4("Data Processing Information"),
        
        textInput('proc', 'Data Processing Software', 
                  value = 'ex. MS-DIAL v4.38'),
        textInput('norm', 'Normalization Method', 
                  value = 'ex. SERRF'),
        
        h4("QC Information"),
        
        textInput('qcn1', p('1st Quality Control Name ',
                            strong("*",
                                   style = "color:red")
                            ),
                  value = 'ex. Pool QC'),
        textInput('qcn2', p('2nd Quality Control Name (if none, type "N/A") ',
                            strong("*",
                                   style = "color:red")
        ),
                  value = 'ex. Bio IVT'),
        textInput('qcn3', p('3rd Quality Control (if none, type "N/A") ',
                            strong("*",
                                   style = "color:red")
        ),
                  value = 'ex. Method Blank')
        ),
    mainPanel(
        
    )
)


#### UI - Downloads ####

ui_dwn <- sidebarLayout(
    sidebarPanel(
        h4('Report'),
        downloadButton("report", "Generate Report"),
        downloadButton("p1", "Download Plots (Must generate report first)"),
        downloadButton("t1", "Download Tables (Must generate report first)")
        ),
    mainPanel(
        
    )
)


#### Combined UI elements ####

ui <- fluidPage(theme = shinythemes::shinytheme("paper"),
        shinyFeedback::useShinyFeedback(),
        titlePanel("MS-QuickQC"),
        ui_instructions,
        ui_upload,
        ui_study,
        ui_dpqc,
        ui_dwn
                )
                  
               
            

#### Server ####

# Define server logic required to draw a histogram
server = function(input, output){
    

    values <- reactiveValues(file2 = data.frame(`No Data Provided` = c("no values",
                                                                       "no values"),
                                                `No Data` = c("no values",
                                                              "no values"))
                            )
    
    ### download folders as zip folder
    
    output$contents1 <- DT::renderDataTable({
        
        req(input$file1)
        
        inFile <- input$file1
        
        DT::datatable(read_excel(inFile$datapath, 1),
                      extensions = c('Buttons','Scroller'),
                      options = list(scrollX = 600)
                    )
            })
    
    
    observeEvent(input$file1,
                                 {
                        values$file2 <- read_excel(input$file1$datapath,
                                              2)
                                 })
    
    
    
        output$contents2 <- DT::renderDataTable({
      
      req(input$file1)
        
        inFile2 <- values$file2
        
        DT::datatable(inFile2,
                      extensions = c('Buttons','Scroller'),
                      options = list(scrollX = 600)
                    )
            })
    
    
    
    
    
    
    
    output$report <- downloadHandler(
        filename = "report.html",
        content = function(file) {
            withProgress(message = 'Please wait while the
                         report is generated...', {
                
                req(input$file1)
                             
                # normalized data input
                
                inrep <- input$file1
                
                #annotation and class input
                
                inclass <- values$file2
                
                
                #radio button input for annotation and msi data sheet

                
                trf_fn <- switch(isolate(input$msi),
                                  y = TRUE,
                                  n = FALSE
                                  )
                
                #parameters to pass into R markdown
                
                params <- list(dat1 = read_excel(inrep$datapath, 
                                                 1),
                               cls1 = inclass,
                               class_msi = trf_fn,
                               
                               
                               std = input$studyname,
                               nm = input$name,
                               chr = input$chro,
                               i1 = input$inst,
                               pl = input$plat,
                               ba = input$batch,
                               dt = input$date,
                               
                               
                               pr = input$proc,
                               nor = input$norm,
                               q1 = input$qcn1,
                               q2 = input$qcn2,
                               q3 = input$qcn3,
                               
                               
                               rendered_by_shiny = T)
                
                rmarkdown::render("QC_report_app_v3_input.Rmd", output_file = file,
                                  params = params,
                                  envir = new.env(parent = globalenv()))
            })
        }
    )
    
    output$p1 <- downloadHandler(
      filename = "plots.zip",
      
      content = function(file) {

        file.copy("plots.zip",
                  file,
                  overwrite = T)
      }
      
      )
    
    
    output$t1 <- downloadHandler(
      filename = "tables.zip",
      
      content = function(file) {
        
        file.copy("tables.zip",
                  file,
                  overwrite = T)
      }
      
    )
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
