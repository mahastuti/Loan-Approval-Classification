# Packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(readxl)              
library(dashboardthemes)    
library(DT)
library(plotly)            
library(dplyr)
library(rsconnect)
#deployApp(appDir = getwd())

# set working directory
#setwd("D:/c/311024_LoanApproval")
getwd()

# load data
df <- read_xlsx("rshiny.xlsx", sheet = 1)

# UI [Header]
header <- dashboardHeader(
  title = tags$span(
    style = "color: #ffcb64; font-size: 23px; font-weight: bold; font-family: 'Kufam', sans-serif;","Loan Approval")
)
# UI [Sidebar]
sidebar <- dashboardSidebar(
  width = 230,
  sidebarMenu(
    menuItem(HTML("&nbsp;&nbsp; Home"), tabName = "home", icon = icon("home")),
    menuItem(HTML("&nbsp;&nbsp; Tabel Data"), tabName = "tabel_data", icon = icon("table")),
    menuItem(HTML("&nbsp;&nbsp; Plot Data"), tabName = "plot_data", icon = icon("chart-line"))
  )
)

# UI [Body]
body <- dashboardBody(
  
  # 01 Custom css 
  tags$head(
    tags$style(HTML("
    
    /* header dan navbar */
    .skin-yellow .main-header .navbar, 
    .skin-yellow .main-header .logo {
    background-color: #2b2257 !important;
    }
    .skin-yellow .main-header .navbar .sidebar-toggle {
    color: #ffcb64 !important;
    }
    .skin-yellow .main-header .navbar .sidebar-toggle:hover {
    background-color: #ffcb64 !important;
    color: #2b2257 !important;
    }
      
    /* sidebar */
    .skin-yellow .main-sidebar {
    background-color: #342a65 !important;
    }
    .skin-yellow .sidebar-menu > li > a {
    color: #ffcb64 !important;
    font-size: 15px;
    font-family: 'Kufam', sans-serif;
    }
    .skin-yellow .sidebar-menu > li:hover > a {
    background-color: #ffcb64 !important;
    color: #342a65 !important;
    font-weight: bold;
    }
    .skin-yellow .sidebar-menu > li.active > a {
    background-color: #ffcb64 !important;
    color: #2b2257 !important;
    }
    .skin-yellow .sidebar-menu > li > a > .fa {
    color: #ffcb64 !important; /* icon color */
    }
      
    /* font semua page */
    body {
    font-family: 'Kufam', sans-serif;
    }
    
    /* button 4 */
    .custom-button {
    background-color: #FF89B3; 
    color: white; 
    border-radius: 10px; 
    padding: 8px 15px; 
    font-size: 16px;
    text-align: center;
    border: none; 
    margin: 0; 
    }
    .custom-button:hover {
    background-color: #FF6492;
    color: #FFF;
    }
    
    /* predict */
    .custom-title {
    text-align: center;
    font-size: 20px;
    font-weight: bold;
    margin-bottom: 20px;
    }
    
    .red-alert {
    color: red;
    font-weight: bold;
    }
    .centered {
    text-align: center;
    }
    
    
    /* custom table data box */
    .dataTable tr:hover {
      background-color: #f1f1f1 !important; /* Warna baris saat hover */
    }
    
    /* predict button */
    .btn-custom {
    font-family: 'Kufam', sans-serif;
    color: white;
    background-color: #007BFF;
    border: none;
    border-radius: 5px;
    padding: 10px 20px;
    font-size: 16px;
    transition: background-color 0.3s ease, transform 0.2s ease;
    cursor: pointer;
    }
    
    .btn-custom:hover {
    background-color: #0056b3;
    transform: scale(1.05);
    
    .center-box {
    display: flex;
    justify-content: center;
    align-items: center;
    height: 100vh;
    }
    
    " 
      )
    )
  ),
  
  # 02 Isi tiap tab
  # home
  tabItems(
    tabItem(tabName = "home",
            
            # judul panel dan deskripsi panel
            titlePanel(
              h1(strong("Loan Approval Prediction"), style = "text-align: center; font-family: 'Kufam'; color: #2b2257;")),
            div(
              p("Is your loan approved or rejected?",style = "text-align:center; font-family: 'Kufam'; color: #2b2257;")),
            br(),
            
            # Embedded video
            tags$iframe(
              style = "border: none; margin: 0 auto; display: block;",
              width = "720", height = "405",
              src = "https://www.youtube.com/embed/Kie-zaqcSuY", 
              frameborder = "0", allowfullscreen = TRUE
            ),
            br(),
            box(title = strong("About Dashboard"), width = 12, solidHeader = TRUE,
              p(
                style = "text-align: justify;",
                "Dashboard ini memiliki 4 tab fitur utama: ",
                strong("Home,"), " untuk memberikan gambaran umum tentang aplikasi; ",
                strong("Dataset,"), " untuk menampilkan data yang digunakan dalam memprediksi status pinjaman berdasarkan parameter seperti usia, pendapatan, jenis kepemilikan rumah, panjang pengalaman kerja, tujuan pinjaman, kelas pinjaman, persentase pendapatan yang digunakan untuk membayar pinjaman, riwayat kredit, dan lain-lain; ",
                strong("Plot Data,"), " untuk visualisasi data yang tersedia; dan ",
                strong("Predict,"), " untuk melakukan prediksi status pinjaman berdasarkan input parameter pengguna."
            )),
            box(title = strong("Variables Influencing Loan Approval"), width = 12, solidHeader = TRUE, style = "margin-bottom: 60px;",
                p(style = "text-align: justify;", strong("person_age :"), "Usia pemohon."),
                p(style = "text-align: justify;", strong("person_income :"), "Pendapatan tahunan pemohon."),
                p(style = "text-align: justify;", strong("person_home_ownership :"), "Jenis kepemilikan rumah (misalnya, Rent, Own, dll.)."),
                p(style = "text-align: justify;", strong("person_emp_length :"), "Panjang pengalaman kerja dalam tahun."),
                p(style = "text-align: justify;", strong("loan_intent :"), "Tujuan utama peminjaman."),
                p(style = "text-align: justify;", strong("loan_grade :"), "Kelas pinjaman."),
                p(style = "text-align: justify;", strong("loan_amnt :"), "Jumlah pinjaman."),
                p(style = "text-align: justify;", strong("loan_int_rate :"), "Suku bunga pinjaman."),
                p(style = "text-align: justify;", strong("loan_percent_income :"), "Persentase pendapatan yang digunakan untuk membayar pinjaman."),
                p(style = "text-align: justify;", strong("cb_person_default_on_file :"), "Riwayat default pemohon di catatan kredit."),
                p(style = "text-align: justify;", strong("cb_person_cred_hist_length :"), "Panjang sejarah kredit."),
                p(style = "text-align: justify;", strong("loan_status :"), "Status pinjaman, apakah disetujui atau tidak (1 = disetujui, 0 = tidak).")
            )
    ),
    
    # tabel
    tabItem(
      tabName = "tabel_data",
      div(
        style = "margin-left: 20px; margin-right: 20px;",
        fluidRow(
          # row 1
          fluidRow(
            infoBox("Oldest Borrower", "144 years old", icon = icon("user-clock"), color = "red"),
            infoBox("Youngest Borrower", "20 years old", icon = icon("user-clock"), color = "yellow"),
            infoBox("Typical Borrower Age", "26 years old", icon = icon("user"), color = "blue")
          ),
          # row 2
          fluidRow(
            infoBox("Highest Recorded Income", "$6,000,000", icon = icon("dollar-sign"), color = "green"),
            infoBox("Usual Income", "$57,000", icon = icon("money-bill-wave"), color = "purple"),
            infoBox("Typical Loan Amount", "$8,000", icon = icon("hand-holding-usd"), color = "teal")
          ),
          # row 3
          fluidRow(
            infoBox("Common Interest Rate", "10.79%", icon = icon("percent"), fill = TRUE, color = "light-blue"),
            infoBox("Usual Employment Length", "4 years", icon = icon("briefcase"), fill = TRUE, color = "orange"),
            infoBox("Common Credit History Length", "4 years", icon = icon("history"), fill = TRUE, color = "navy")
          )
        )
      ),
      
      br(),
      fluidRow(
        box(
          width = 12,
          solidHeader = TRUE,
          status = "info",
          style = "margin-bottom: 20px; position: relative;", 
          title = "Dataset",
          tags$div(
            style = "margin-bottom: 10px; text-align: right;",
            downloadButton(
              outputId = "download_data",
              label = "Download Data",
              style = "background-color: #007BFF; color: white; border: none; border-radius: 4px; padding: 8px 16px; font-size: 14px;"
            )
          ),
          DT::dataTableOutput("data_table")
        )
      )
    ),
    
    # plot
    tabItem(
      tabName = "plot_data",
      div(h1(strong("Data Visualization"), style = "text-align: center; margin-bottom: 0px; font-family: 'Kufam'; color: #2b2257;")),
      br(),
      fluidRow(
        box(
          title = "Graphic Settings", solidHeader = TRUE, status = "primary", width = 12,
          selectInput("xcol", "", choices = names(df)),
          selectInput("group", "Loan Status:", choices = c("All","Approved", "Rejected"))
        )
      ),
      fluidRow(
        box(
          title = "Graphic", width = 12, solidHeader = TRUE, status = "success",
          tabsetPanel(
            tabPanel("Chart", plotlyOutput("chart", height = "100%", width = "100%")), 
            tabPanel("Heatmap", plotlyOutput("heatmapPlot", height = "1000", width = "100%")) 
          )
        )
      )
    )
  ),
  
  # 03 Footer
  tags$div(
    style = "
    position: fixed;
    bottom: 0;
    left: 0;
    width: 100%;
    background-color: #2b2257;
    color: #ffcb64;
    text-align: center;
    padding: 10px;
    font-size: 14px;
    font-family: 'Kufam', sans-serif;
    z-index: 1000;",
    "© 2024 Loan Approval Dashboard | Nalini Mahastuti Panunjul"
  )
)

# UI [Dashboard Page]
ui <- dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "yellow"
)

# Server
server <- function(input, output, session) {
  
  # 01 table output
  output$data_table <- renderDataTable({
    datatable(df, options = list( scrollX = TRUE, pageLength = 100, lengthMenu = c(50, 100, 200, 1000), autoWidth = TRUE),
    rownames = FALSE)
  })
  
  # 02 chart
  output$chart <- renderPlotly({
    data_filtered <- if (input$group == "All") {
      df
    } else {
      df %>% filter(loan_status == ifelse(input$group == "Approved", 1, 0))
    }
    
    if (is.numeric(data_filtered[[input$xcol]])) {
      plot_ly(data_filtered, x = ~get(input$xcol), type = 'histogram') %>%
        layout(
          title = paste("Distribution of", input$xcol, "for", input$group, "Loans"),
          xaxis = list(title = input$xcol),
          yaxis = list(title = "Frequency"),
          margin = list(l = 50, r = 50, b = 50, t = 50)
        )
    } else {
      data_grouped <- data_filtered %>%
        group_by(across(all_of(input$xcol))) %>%
        summarise(count = n(), .groups = 'drop')
      
      plot_ly(data = data_grouped, x = ~get(input$xcol), y = ~count, type = 'bar') %>%
        layout(
          title = paste("Count of", input$xcol, "for", input$group, "Loans"),
          xaxis = list(title = input$xcol),
          yaxis = list(title = "Count"),
          margin = list(l = 50, r = 50, b = 50, t = 50)
        )
    }
  })
  
  # heatmap
  output$heatmapPlot <- renderPlotly({
    data_filtered <- if (input$group == "All") {
      df 
    } else {
      df %>% filter(loan_status == ifelse(input$group == "Approved", 1, 0))
    }
    
    encoded_df <- data_filtered %>%
      select(-loan_status) %>%
      mutate_if(is.factor, as.character) %>%
      mutate_if(is.character, ~as.numeric(as.factor(.)))
    
    numeric_cols <- encoded_df %>% select(where(is.numeric))
    
    if (ncol(numeric_cols) > 1) {
      correlation_matrix <- cor(numeric_cols, use = "complete.obs")
      
      plot_ly(
        z = ~correlation_matrix,
        x = colnames(correlation_matrix),
        y = colnames(correlation_matrix),
        type = "heatmap",
        colorscale = "RdYlBu",
        colorbar = list(
          tickvals = c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1),
          ticktext = c("-1", "-0.75", "-0.5", "-0.25", "0", "0.25", "0.5", "0.75", "1")
        )
      ) %>% layout(
        title = "Correlation Heatmap",
        margin = list(l = 50, r = 50, b = 100, t = 50)
      )
    } else {
      plotly_empty() %>% layout(
        title = "Insufficient numerical data for heatmap",
        showlegend = FALSE,
        margin = list(l = 50, r = 50, b = 50, t = 50)
      )
    }
  })
  
  # box atasnya data
  output$progressBox <- renderInfoBox({
    infoBox("Progress", paste0(25, "%"), icon = icon("list"), color = "purple")
  })
  
  output$approvalBox <- renderInfoBox({
    infoBox("Approvals", 80, icon = icon("thumbs-up"), color = "green")
  })
  
  output$progressBox2 <- renderInfoBox({
    infoBox("Progress (Row 2)", paste0(45, "%"), icon = icon("list"), fill = TRUE, color = "blue")
  })
  
  output$approvalBox2 <- renderInfoBox({
    infoBox("Approvals (Row 2)", 90, icon = icon("check-circle"), fill = TRUE, color = "yellow")
  })
  
  # download dataset
  output$download_data <- downloadHandler(
    filename = function() {
      paste("data_table-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_table_data(), file, row.names = FALSE)
    }
  )
}

# run
shinyApp(ui, server)
