library(shiny)
library(bs4Dash)
library(readxl)
library(dplyr)
library(DT)
library(plotly)
library(GGally)
library(ggplot2)
library(broom)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(leaflet)
library(car)
library(lmtest)

data_dashboard <-  read_excel("C:/BERKAS STIS/TINGKAT 2/2KS3/SEMESTER 4/KOMSTAT/PROJEK KOMSTAT/datakomstat.xlsx")
peta_indonesia <- st_read("C:/BERKAS STIS/TINGKAT 2/2KS3/SEMESTER 4/KOMSTAT/PROJEK KOMSTAT/38 Provinsi Indonesia - Provinsi.json")

variabel_iklim <- c("Suhu Rata-Rata" = "suhu_rata2",
                    "Curah Hujan" = "curah_hujan",
                    "Kelembaban" = "kelembaban", 
                    "Kecepatan Angin" = "kecepatan_angin",
                    "Tekanan Udara" = "tekanan_udara")

numeric_vars <- names(data_dashboard[c(-1,-3,-12:-17)])[sapply(data_dashboard[c(-1,-3,-12:-17)], is.numeric)]

format_label <- function(x) {
  sapply(x, function(n) {
    n_clean <- gsub("_", " ", n)
    words <- strsplit(n_clean, " ")[[1]]
    paste(toupper(substring(words, 1,1)), tolower(substring(words, 2)), sep="", collapse=" ")
  }, USE.NAMES = FALSE)
}

ui <- dashboardPage(
  title = "Climate Change",
  help = NULL,
  fullscreen = TRUE,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Climate Change",
      image = "https://cdn-icons-png.flaticon.com/512/2938/2938122.png",
      color = "primary"
    )
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebarMenuid",
      menuItem(
        "Beranda",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("chart-line"),
        menuSubItem(
          "Menurut Provinsi",
          tabName = "dashboard-provinsi",
        ),
        menuSubItem(
          "Menurut Bencana Alam",
          tabName = "dashboard-bencana"
        )
      ),
      menuItem(
        "Analisis",
        tabName = "analisis",
        icon = icon("calculator"),
        menuSubItem(
          "Analisis Korelasi",
          tabName = "analisis-korelasi",
          icon = icon("project-diagram")
        ),
        menuSubItem(
          "Analisis Regresi",
          tabName = "analisis-regresi",
          icon = icon("chart-line")
        )
      ),
      menuItem(
        "Tabel Dinamis",
        tabName = "statistik",
        icon = icon("table"),
        menuSubItem(
          "Menurut Provinsi",
          tabName = "stat-wilayah"
        ),
        menuSubItem(
          "Menurut Bencana Alam",
          tabName = "stat-bencana"
        )
      ),
      menuItem(
        "Profile",
        tabName = "profile",
        icon = icon("user")
      )
    )
  ),
  footer = dashboardFooter(
    left = "Dashboard Kelompok (Sekian) Kelas 2KS3",
    right = format(Sys.Date(), "Copyright Politeknik Statistika STIS %Y")
  ),
  body = dashboardBody(tags$head(
    tags$style(HTML("
      /* Hover efek tombol Menuju Dashboard */
      #to_dashboard_button:hover {
        background-color: #218838 !important;
        transform: scale(1.05);
        box-shadow: 0 8px 20px rgba(40, 167, 69, 0.6);
      }

      /* Hover efek tombol Informasi Lebih Lanjut */
      #more_info_button:hover {
        background-color: #218838 !important;
        color: #fff !important;
        transform: scale(1.05);
      }

      #to_dashboard_button, #more_info_button {
        cursor: pointer;
      }
      
      select[multiple] {
        scrollbar-width: thin;
        scrollbar-color: #888 #eee;
      }
      select[multiple]::-webkit-scrollbar {
        width: 8px;
      }
      select[multiple]::-webkit-scrollbar-thumb {
        background-color: #888;
        border-radius: 4px;
      }
    "))
  ),
  tabItems(
    tabItem(
      tabName = "home",
      box(
        title = NULL,
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        div(
          class = "text-center",
          style = "padding: 40px 20px; background: linear-gradient(135deg, #00c6ff 0%, #0072ff 100%);
             border-radius: 15px; color: white; margin: -15px; box-shadow: 0 10px 30px rgba(0,0,0,0.2);",
          
          div(
            style = "margin-bottom: 30px;",
            icon("cloud-sun", style = "font-size: 48px; color: #ffd700; margin-bottom: 15px;"),
            h1("Selamat Datang!", 
               style = "font-family: 'Arial', sans-serif; font-weight: bold; margin-bottom: 10px; text-shadow: 2px 2px 4px rgba(0,0,0,0.3);")
          ),
          
          div(
            style = "margin-bottom: 35px; max-width: 800px; margin-left: auto; margin-right: auto;",
            p("Dashboard ini menyediakan visualisasi perubahan iklim di Indonesia dan dampaknya terhadap korban bencana alam dalam kurun waktu 2020–2024",
              style = "font-size: 18px; line-height: 1.6; color: #f8f9fa; text-shadow: 1px 1px 2px rgba(0,0,0,0.3);")
          ),
          
          div(
            style = "margin-bottom: 20px;",
            actionButton("to_dashboard_button", 
                         "Menuju Dashboard", 
                         class = "btn-lg",
                         style = "background: #28a745; border: none; padding: 15px 40px; 
                           font-size: 18px; font-weight: bold; border-radius: 25px; 
                           box-shadow: 0 5px 15px rgba(40, 167, 69, 0.4); 
                           transition: all 0.3s ease; color: white;")
          ),
          
          div(
            style = "margin-top: 15px;",
            actionButton("more_info_button", 
                         "Informasi Lebih Lanjut", 
                         icon = icon("info-circle"), 
                         style = "background:none ; border: 2px solid white;
                           color: white; padding: 10px 25px; border-radius: 20px; 
                           transition: all 0.3s ease;")
          ),
          
          br(),
          uiOutput("more_info_ui")
        )
      )
    ),
    
    tabItem(
      tabName = "dashboard-provinsi",
      
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = "provinsi_terpilih",
            label = "Pilih Provinsi :",
            choices = c("Indonesia", unique(data_dashboard$provinsi)),
            selected = "Indonesia",
            width = "100%"
          )
        ),
        
        column(
          width = 4,
          sliderInput(
            inputId = "rentang_tahun_provinsi",
            label = "Pilih Rentang Tahun :",
            min = min(data_dashboard$tahun),
            max = max(data_dashboard$tahun),
            value = c(min(data_dashboard$tahun), max(data_dashboard$tahun)),
            sep = ""
          )
        ) 
      ), 
      
      
      fluidRow(
        infoBoxOutput("suhu_box", width = 4),
        infoBoxOutput("Total_Korban_box", width = 4),
        infoBoxOutput("jumlah_bencana_box", width = 4)
      ),
      
      
      fluidRow(
        box(
          title = "Trend Korban Jiwa per Tahun",
          status = "danger",
          solidHeader = TRUE,
          width = 6,
          plotlyOutput("trend_korban_plot")
        ),
        box(
          title = "Trend Jumlah Bencana per Tahun",
          status = "warning",
          solidHeader = TRUE,
          width = 6,
          plotlyOutput("trend_bencana_plot")
        )
      ),
      
      fluidRow(
        box(
          title = "Trend Iklim per Tahun",
          status = "gray",
          solidHeader = TRUE,
          width = 6,
          selectInput(
            inputId = "variabel_iklim",
            label = "Pilih Variabel Iklim : ",
            choices = variabel_iklim,
            selected = "Suhu Rata-Rata"
          ),
          plotlyOutput("trend_iklim_plot")
        ),
        box(
          title = "Bencana Alam yang Memakan Korban Jiwa Terbanyak",
          status = "primary",
          solidHeader = TRUE,
          width = 6,
          plotlyOutput("top_bencana_plot")
        )
      )
    ),
    
    tabItem(
      tabName = "dashboard-bencana",
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = "bencana_terpilih",
            label = "Pilih Bencana :",
            choices = c("Banjir", "Cuaca ekstrem", "Gelombang pasang / Abrasi", "Kebakaran hutan dan lahan", "Kekeringan", "Longsor"),
            selected = "Banjir",
            width = "100%"
          )
        ),
        
        column(
          width = 4,
          sliderInput(
            inputId = "rentang_tahun_bencana",
            label = "Pilih Rentang Tahun :",
            min = min(data_dashboard$tahun),
            max = max(data_dashboard$tahun),
            value = c(min(data_dashboard$tahun), max(data_dashboard$tahun)),
            sep = ""
          )
        ) 
      ),
      
      fluidRow(
        infoBoxOutput("korban_spesifik_box", width = 4),
        infoBoxOutput("provinsi_terdampak_box", width = 4),
        infoBoxOutput("bencana_spesifik_box", width = 4)
      ),
      
      fluidRow(
        column(
          width = 6,
          uiOutput("trend_korban_spesifik_box")
        ),
        column(
          width = 6,
          uiOutput("trend_bencana_spesifik_box")
        )
      ),
      
      fluidRow(
        column(
          width = 12,
          uiOutput("peta_distribusi_box")
        )
      )
    ),
    
    tabItem(
      tabName = "analisis-korelasi",
      fluidRow(
        box(
          width = 4, status = "primary", solidHeader = TRUE,
          title = "Pilih Variabel (≥2)",
          checkboxGroupInput(
            inputId  = "vars_korelasi",
            label    = NULL,
            choices  = setNames(numeric_vars, format_label(numeric_vars)),
            selected = numeric_vars[1:2]
          )
        ),
        box(
          width = 8, status = "info", solidHeader = TRUE,
          title = "Scatter‑matrix",
          plotOutput("scatterplot")
        )
      ),
      
      fluidRow(
        box(
          width = 12, status = "success", solidHeader = TRUE,
          title = "Matriks Korelasi",
          DTOutput("cor_matrix")
        )
      )
    ),
    
    tabItem(
      tabName = "analisis-regresi",
      fluidRow(
        box(
          width = 4, status = "primary", solidHeader = TRUE,
          title = "Pilih Variabel",
          selectInput(
            inputId  = "dep_var",
            label    = "Variabel Dependen (Y):",
            choices  = setNames(numeric_vars[c(-2,-9:-13)], format_label(numeric_vars)[c(-2,-9:-13)]),
            selected = ifelse("Banjir" %in% numeric_vars, "Banjir", numeric_vars[1])
          ),
          selectizeInput(
            inputId  = "indep_vars",
            label    = "Variabel Independen (X):",
            choices  = setNames(setdiff(numeric_vars, ifelse("Banjir" %in% numeric_vars, "Banjir", numeric_vars[1])),
                                format_label(setdiff(numeric_vars, ifelse("Banjir" %in% numeric_vars, "Banjir", numeric_vars[1])))),
            selected = setdiff(numeric_vars, ifelse("Banjir" %in% numeric_vars, "Banjir", numeric_vars[1]))[1:3],
            multiple = TRUE,
            options  = list(
              plugins = list("remove_button"),   # ada tombol x di tiap tag
              maxItems = NULL,                   # tak dibatasi jumlah
              dropdownMaxHeight = "300px"        # tinggi dropdown; scroll bar bisa di‑drag
            )
          )
        ),
        box(
          width = 8, status = "info", solidHeader = TRUE,
          title = "Ringkasan Model Regresi",
          verbatimTextOutput("model_summary")
        )
      ),
      
      fluidRow(
        box(
          width = 12, status = "warning", solidHeader = TRUE,
          title = "Diagnostik Plot",
          plotOutput("diagnostic_plot")
        )
      ),
      
      fluidRow(
        box(
          width = 12, status = "info", solidHeader = TRUE,
          title = "Pemeriksaan Asumsi Regresi",
          verbatimTextOutput("asumption")
        )
      )
    ),
    
    tabItem(
      tabName = "stat-wilayah",
      fluidRow(
        box(
          title = "Data Statistik per Provinsi",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          fluidRow(
            column(
              width = 5,
              selectInput(
                inputId = "provinsi_terpilih_stat",
                label = "Pilih Provinsi:",
                choices = c("Indonesia", unique(data_dashboard$provinsi)),
                selected = "Indonesia"
              )
            ),
            column(
              width = 4,
              sliderInput(
                inputId = "rentang_tahun_provinsi_stat",
                label = "Pilih Rentang Tahun :",
                min = min(data_dashboard$tahun),
                max = max(data_dashboard$tahun),
                value = c(min(data_dashboard$tahun), max(data_dashboard$tahun)),
                sep = ""
              )
            ),
            column(
              width = 3,
              div(
                style = "text-align: right; padding-top: 25px;",
                downloadButton("download_data_provinsi", "Unduh Data (.csv)")
              )
            )
          ),
          hr(),
          DT::dataTableOutput("tabel_stat_provinsi")
        )
      )
    ),
    
    tabItem(
      tabName = "stat-bencana",
      box(
        title = "Data Statistik per Jenis Bencana",
        status = "info",
        solidHeader = TRUE,
        width = 12,
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = "bencana_terpilih_stat",
              label = "Pilih Bencana:",
              choices = c("Banjir", "Cuaca ekstrem", "Gelombang pasang / Abrasi", 
                          "Kebakaran hutan dan lahan", "Kekeringan", "Longsor"),
              selected = "Banjir"
            )
          ),
          
          column(
            width = 8,
            div(
              style = "text-align: right; padding-top: 25px;",
              downloadButton("download_data_bencana", "Unduh Data (.csv)")
            )
          )
        ),
        hr(),
        DT::dataTableOutput("tabel_stat_bencana")
      )
    ),
    
    tabItem(
      tabName = "profile",
      tags$style("
        .profile-container {
          padding: 20px;
        }
        
        /* Profile Card Style */
        .profile-card {
          background: white;
          border: none;
          border-radius: 20px;
          box-shadow: 0 10px 30px rgba(0,0,0,0.1);
          padding: 30px 20px;
          margin-bottom: 20px;
          transition: all 0.3s ease;
          position: relative;
          overflow: hidden;
        }
        
        .profile-card::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 4px;
          background: linear-gradient(90deg, #667eea, #764ba2);
        }
        
        .profile-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 15px 40px rgba(0,0,0,0.15);
        }
        
        /* Profile Images - Overview Page */
        .profile-card img:not(.detail-image) {
          border-radius: 50% !important;
          border: 3px solid #f8f9fa;
          box-shadow: 0 5px 15px rgba(0,0,0,0.1);
          transition: all 0.3s ease;
          max-width: 120px;
          height: 120px;
          object-fit: cover;
        }
        
        .profile-card img:not(.detail-image):hover {
          transform: scale(1.05);
          box-shadow: 0 8px 25px rgba(0,0,0,0.2);
        }
        
        /* Detail Panel */
        .detail-panel {
          padding: 40px 30px !important;
          min-height: 400px;
        }
        
        /* Container untuk gambar detail */
        .detail-image-container {
          display: flex;
          justify-content: center;
          align-items: flex-start;
          padding: 20px;
        }
        
        /* Gambar di halaman detail*/
        .detail-image {
          border-radius: 20px !important;
          border: 4px solid #f8f9fa;
          box-shadow: 0 10px 30px rgba(0,0,0,0.2);
          transition: all 0.3s ease;
          max-width: 250px !important;
          width: 100% !important;
          height: 250px !important;
          object-fit: cover;
          margin-bottom: 0 !important;
        }
        
        .detail-image:hover {
          transform: scale(1.02);
          box-shadow: 0 15px 40px rgba(0,0,0,0.3);
        }
        
        /* Info section di detail */
        .detail-info {
          padding: 20px 0;
          height: 100%;
          display: flex;
          flex-direction: column;
          justify-content: flex-start;
        }
        
        /* Nama di halaman detail */
        .detail-name {
          color: #2c3e50 !important;
          font-size: 30px !important;
          font-weight: 700 !important;
          margin-bottom: 30px !important;
          text-align: left !important;
          padding-bottom: 15px !important;
          border-bottom: 3px solid #667eea !important;
        }
        
        /* Item detail */
        .detail-item {
          margin-bottom: 15px;
          padding: 10px 0;
          border-bottom: 1px solid #ecf0f1;
        }
        
        .detail-item p {
          font-size: 15px !important;
          margin: 0 !important;
          color: #34495e;
        }
        
        .detail-item strong {
          color: #667eea !important;
          font-weight: 600 !important;
          font-size: 15px !important;
          min-width: 140px !important;
          display: inline-block;
        }
        
        /* Back button khusus */
        .back-btn {
          background: linear-gradient(45deg, #e74c3c, #c0392b) !important;
          color: white !important;
          border: none !important;
          border-radius: 25px !important;
          padding: 12px 30px !important;
          font-weight: 600 !important;
          font-size: 1rem !important;
          margin-top: 20px !important;
          transition: all 0.3s ease !important;
        }
        
        .back-btn:hover {
          background: linear-gradient(45deg, #d62c1a, #a93226) !important;
          transform: translateY(-2px) !important;
          box-shadow: 0 6px 20px rgba(231, 76, 60, 0.4) !important;
          color: white !important;
        }
        
        /* Headings dalam Card - Overview */
        .profile-card h4 {
          color: #2c3e50;
          font-size: 18px;
          font-weight: 600;
          margin: 15px 0 10px 0;
          line-height: 1.3;
        }
        
        /* Paragraf dalam Card - Overview */
        .profile-card p:not(.detail-item p) {
          color: #7f8c8d;
          font-size: 14px;
          margin-bottom: 15px;
          font-weight: 500;
        }
        
        /* Action Buttons - Overview */
        .btn-profile {
          border: none;
          border-radius: 25px;
          padding: 10px 25px;
          font-weight: 600;
          font-size: 0.9rem;
          transition: all 0.3s ease;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          background: linear-gradient(45deg, #667eea, #764ba2);
          color: white;
          box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3);
        }
        
        .btn-profile:hover {
          background: linear-gradient(45deg, #5a6fd8, #6a4190);
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(102, 126, 234, 0.4);
          color: white;
        }
        
        /* Animation untuk transisi halaman */
        .profile-content {
          animation: fadeIn 0.5s ease-in;
        }
        
        @keyframes fadeIn {
          from {
            opacity: 0;
            transform: translateY(20px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        
        /* Responsive Design */
        @media (max-width: 768px) {
          .profile-card {
            padding: 20px 15px;
            margin-bottom: 15px;
          }
          
          .detail-panel {
            padding: 20px 15px !important;
          }
          
          .detail-image {
            max-width: 200px !important;
            height: 200px !important;
          }
          
          .detail-name {
            font-size: 24px !important;
            text-align: center !important;
          }
          
          .detail-info {
            text-align: center;
          }
          
          .detail-item strong {
            min-width: auto !important;
            display: block !important;
            margin-bottom: 5px;
          }
        }
      "),
      div(class = "profile-container",
          uiOutput("profileUI")
      )
    )
  )
  )
)

server <- function(input, output,session){
  
  # Output Dashboard Provinsi
  
  ## Filter Data Reaktif untuk Dashboard Provinsi
  
  provinsi_filter <- reactive({
    if (input$provinsi_terpilih == "Indonesia"){
      data_dashboard %>% 
        filter(tahun >= input$rentang_tahun_provinsi[1] & tahun <= input$rentang_tahun_provinsi[2])
    } else{
      data_dashboard %>%
        filter(provinsi == input$provinsi_terpilih) %>% 
        filter(tahun >= input$rentang_tahun_provinsi[1] & tahun <= input$rentang_tahun_provinsi[2])
    }
  })
  
  ## Suhu Rata-Rata
  output$suhu_box <- renderInfoBox({
    
    suhu_rata <- provinsi_filter() %>%
      summarise(rata2 = mean(suhu_rata2, na.rm = TRUE)) %>%
      pull(rata2)
    
    infoBox(
      title = "Suhu Rata-Rata",
      value = format(round(suhu_rata, 2), nsmall = 2, big.mark = ","),
      icon = icon("temperature-half"),
      color = "primary"
    )
  })
  
  ## Jumlah Korban Bencana Alam
  output$Total_Korban_box <- renderInfoBox({
    
    total_korban <- provinsi_filter() %>%
      summarise(total = sum(Total_Korban, na.rm = TRUE)) %>%
      pull(total)
    
    infoBox(
      title = "Total Korban Jiwa",
      value = format(total_korban, big.mark = ","),
      icon = icon("skull-crossbones"),
      color = "danger"
    )
  })
  
  ## Jumlah Bencana Alam
  output$jumlah_bencana_box <- renderInfoBox({
    
    total_bencana <- provinsi_filter() %>%
      summarise(total = sum(Total_Kejadian, na.rm = TRUE)) %>%
      pull(total)
    
    infoBox(
      title = "Total Bencana Alam",
      value = format(round(total_bencana,0), big.mark = ","),
      icon = icon("volcano"),
      color = "warning"
    )
  })
  
  ## Tren Korban
  output$trend_korban_plot <- renderPlotly({
    plot_data <- provinsi_filter() %>%
      group_by(tahun)%>%
      summarise(total_korban = sum(Total_Korban),.groups = 'drop')
    
    min_y <- min(plot_data$total_korban, na.rm = TRUE) * 0.8
    max_y <- max(plot_data$total_korban, na.rm = TRUE) * 1.1
    
    plot_ly(
      data = plot_data,
      x = ~tahun,
      y = ~total_korban,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = '#C93C3C'), # Danger color
      marker = list(color = '#C93C3C'),
      hoverinfo = 'x+y',
      hovertemplate = 'Tahun: %{x}<br>Korban Jiwa: %{y:,}<extra></extra>'
    ) %>%
      layout(
        xaxis = list(title = "Tahun",dtick=1),
        yaxis = list(title = "Jumlah Korban Jiwa", range = c(min_y,max_y))
      )
  })
  
  ## Tren Bencana
  output$trend_bencana_plot <- renderPlotly({
    
    plot_data <- provinsi_filter() %>%
      group_by(tahun) %>%
      summarise(total_bencana = sum(Total_Kejadian, na.rm = TRUE), .groups = 'drop')
    
    min_y <- min(plot_data$total_bencana, na.rm = TRUE) * 0.8
    max_y <- max(plot_data$total_bencana, na.rm = TRUE) * 1.1
    
    plot_ly(
      data = plot_data,
      x = ~tahun,
      y = ~total_bencana,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = '#FF851B'), # Warning color
      marker = list(color = '#FF851B'),
      hoverinfo = 'x+y',
      hovertemplate = 'Tahun: %{x}<br>Jumlah Bencana: %{y:,}<extra></extra>'
    ) %>%
      layout(
        xaxis = list(title = "Tahun",dtick=1),
        yaxis = list(title = "Jumlah Kejadian Bencana", range = c(min_y,max_y))
      )
  })
  
  ## Tren Iklim
  output$trend_iklim_plot <- renderPlotly({
    y_axis <- names(variabel_iklim)[variabel_iklim == input$variabel_iklim]
    
    plot_data <- provinsi_filter() %>% 
      group_by(tahun) %>%
      summarise(nilai_rata2 = mean(.data[[input$variabel_iklim]], na.rm = TRUE))
    
    min_y <- min(plot_data$nilai_rata2, na.rm = TRUE) * 0.98
    max_y <- max(plot_data$nilai_rata2, na.rm = TRUE) * 1.02
    
    plot_ly(
      data = plot_data,
      x = ~tahun,
      y = ~nilai_rata2,
      type = 'scatter',
      mode = 'lines+markers',
      hovertemplate = paste(
        'Tahun: %{x}<br>',
        'Nilai Rata-rata: %{y:.2f}<extra></extra>'
      )
    ) %>% 
      layout(
        title = paste("Tren Tahunan",y_axis),
        xaxis = list(title = "Tahun", dtick = 1),
        yaxis = list(title = y_axis, range = c(min_y,max_y))
      )
  })
  
  ## Top Bencana
  output$top_bencana_plot <- renderPlotly({
    
    plot_data <- provinsi_filter() %>%
      select(Banjir, `Cuaca ekstrem`, `Gelombang pasang / Abrasi`, `Kebakaran hutan dan lahan`, Kekeringan, Longsor) %>%
      summarise_all(sum, na.rm = TRUE) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Jenis_Bencana",
        values_to = "Total_Kejadian"
      ) %>%
      arrange(desc(Total_Kejadian)) %>% 
      slice_head(n = 3)
    
    if (nrow(plot_data) == 0 || sum(plot_data$Total_Kejadian) == 0) {
      return(
        plot_ly() %>%
          layout(
            title = "Tidak ada data kejadian untuk ditampilkan pada pilihan ini",
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
      )
    }
    
    plot_ly(
      data = plot_data,
      y = ~reorder(Jenis_Bencana, Total_Kejadian),
      x = ~Total_Kejadian,
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#007BFF'),
      hovertemplate = '%{y}<br>Jumlah Kejadian: %{x:,}<extra></extra>'
    ) %>%
      layout(
        title = "Jenis Bencana dengan Jumlah Kejadian Terbanyak",
        xaxis = list(title = "Total Kejadian"),
        yaxis = list(title = "")
      )
  })
  
  # Output Dashboard Bencana
  
  ## Filter Data Reaktif untuk Dashboard Bencana
  
  bencana_filter <- reactive({
    bencana_spesifik <- input$bencana_terpilih
    korban_spesifik <- paste0("korban_",gsub(" / ","_", bencana_spesifik))
    korban_spesifik <- gsub(" ","_", korban_spesifik)
    
    data_dashboard %>%
      filter(tahun >= input$rentang_tahun_bencana[1] & tahun <= input$rentang_tahun_bencana[2]) %>% 
      select(
        provinsi,
        tahun,
        bencana_spesifik = all_of(bencana_spesifik),
        korban_spesifik = all_of(korban_spesifik)
      )
  })
  
  ## Jumlah Korban Bencana Spesifik
  output$korban_spesifik_box <- renderInfoBox({
    total_korban <- sum(bencana_filter()$korban_spesifik,na.rm = TRUE)
    
    infoBox(
      title = paste("Total Korban Jiwa", input$bencana_terpilih),
      value = format(total_korban,big.mark = ","),
      icon = icon("skull-crossbones"),
      color = "danger"
    )
  })
  
  ## Jumlah Kejadian Bencana Spesifik
  output$bencana_spesifik_box <- renderInfoBox({
    total_kejadian <- sum(bencana_filter()$bencana_spesifik,na.rm = TRUE)
    
    infoBox(
      title = paste("Total Kejadian",input$bencana_terpilih),
      value = format(total_kejadian,big.mark=","),
      icon = icon("volcano"),
      color = "info"
    )
  })
  
  ## Provinsi Paling Terdampak
  output$provinsi_terdampak_box <-  renderInfoBox({
    prov_terdampak <- bencana_filter() %>% 
      group_by(provinsi) %>% 
      summarise(total_kejadian = sum(bencana_spesifik, na.rm = TRUE)) %>% 
      arrange(desc(total_kejadian)) %>% 
      slice (1) %>% 
      pull (provinsi)
    
    if (length(prov_terdampak) == 0){
      prov_terdampak <- "N/A"
    }
    
    infoBox(
      title = "Provinsi Paling Terdampak",
      value = prov_terdampak,
      icon = icon("map-location-dot"),
      color = "warning"
    )
  })
  
  ## Tren Korban Bencana Spesifik
  output$trend_korban_spesifik_box <- renderUI({
    box(
      title = paste("Tren Korban Jiwa Akibat", input$bencana_terpilih),
      status = "danger",
      solidHeader = TRUE,
      width = 12,
      plotlyOutput("plot_trend_korban_spesifik", height = "350px")
    )
  })
  
  output$plot_trend_korban_spesifik <- renderPlotly({
    plot_data <- bencana_filter() %>% 
      group_by(tahun) %>% 
      summarise(total_per_tahun = sum(korban_spesifik, na.rm = TRUE))
    
    min_y <- min(plot_data$total_per_tahun, na.rm = TRUE) * 0.98
    max_y <- max(plot_data$total_per_tahun, na.rm = TRUE) * 1.02
    
    plot_ly(
      plot_data,
      x = ~tahun,
      y = ~total_per_tahun,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = '#C93C3C'),
      marker = list(color = '#C93C3C'),
      hoverinfo = 'x+y',
      hovertemplate = 'Tahun: %{x}<br>Jumlah Korban: %{y:,}<extra></extra>'
    ) %>% 
      layout(
        xaxis = list(title = "Tahun", dtick = 1),
        yaxis = list(title = "Jumlah Korban Jiwa", range = c(min_y,max_y))
      )
  })
  
  ## Tren Kejadian Bencana Spesifik
  output$trend_bencana_spesifik_box <- renderUI({
    box(
      title = paste("Tren Kejadian", input$bencana_terpilih),
      status = "info",
      solidHeader = TRUE,
      width = 12,
      plotlyOutput("plot_trend_bencana_spesifik", height = "350px")
    )
  })
  
  output$plot_trend_bencana_spesifik <- renderPlotly({
    plot_data <- bencana_filter() %>% 
      group_by(tahun) %>% 
      summarise(total_per_tahun = sum(bencana_spesifik, na.rm = TRUE))
    
    min_y <- min(plot_data$total_per_tahun, na.rm = TRUE) * 0.96
    max_y <- max(plot_data$total_per_tahun, na.rm = TRUE) * 1.04
    
    plot_ly(
      plot_data,
      x = ~tahun,
      y = ~total_per_tahun,
      type = 'scatter',
      mode = 'lines+markers',
      hoverinfo = 'x+y',
      hovertemplate = 'Tahun: %{x}<br>Jumlah Bencana: %{y:,}<extra></extra>'
    ) %>% 
      layout(
        xaxis = list(title = "Tahun", dtick = 1),
        yaxis = list(title = "Jumlah Kejadian", range = c(min_y, max_y))
      )
  })
  
  ## Peta dengan Leaflet
  output$peta_distribusi_box<- renderUI({
    box(
      title = paste("Peta Distribusi", input$bencana_terpilih),
      status = "success",
      solidHeader = TRUE,
      width = 12,
      leafletOutput("peta_distribusi", height = "400px")
    )
  })
  
  output$peta_distribusi <- renderLeaflet({
    data_leaflet <-  bencana_filter() %>% 
      group_by(provinsi) %>% 
      summarise(total_bencana = sum(bencana_spesifik, na.rm = TRUE)) %>% 
      filter(total_bencana > 0)
    
    peta_indonesia <-  left_join(peta_indonesia,data_leaflet, by = c("PROVINSI" = "provinsi"))
    
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = peta_indonesia$total_bencana
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%d kejadian",
      peta_indonesia$PROVINSI,
      peta_indonesia$total_bencana
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data = peta_indonesia) %>% 
      addTiles() %>% 
      setView(lng = 118, lat = -2, zoom = 5) %>% 
      addPolygons(
        fillColor = ~pal(total_bencana),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>% 
      addLegend(
        pal = pal,
        values = ~total_bencana,
        opacity = 0.7,
        title = paste("Jumlah Kejadian",input$bencana_terpilih),
        position = "bottomright"
      )
  })
  
  # Output Analisis
  
  ## Korelasi
  
  output$scatterplot <- renderPlot({
    req(input$vars_korelasi)
    validate(need(length(input$vars_korelasi) >= 2, "Pilih minimal 2 variabel."))
    
    # Ambil subset data sesuai pilihan, lalu ubah label kolom hanya untuk tampilan
    plot_data <- data_dashboard[, input$vars_korelasi, drop = FALSE]
    colnames(plot_data) <- format_label(colnames(plot_data))
    
    ggpairs(plot_data)
  })
  
  output$cor_matrix <- renderDT({
    req(input$vars_korelasi)
    validate(need(length(input$vars_korelasi) >= 2, ""))
    
    plot_data <- data_dashboard[, input$vars_korelasi, drop = FALSE]
    cor_mat   <- round(cor(plot_data, use = "pairwise.complete.obs"), 3)
    colnames(cor_mat) <- rownames(cor_mat) <- format_label(colnames(cor_mat))
    
    datatable(cor_mat, options = list(dom = "t", scrollX = TRUE))
  })
  
  # Regresi
  
  # Default X untuk masing-masing Y
  default_x_map <- list(
    "Total_Kejadian" = c("suhu_rata2", "curah_hujan", "kelembaban", "kecepatan_angin", "tekanan_udara"),
    "Banjir" = c("curah_hujan"),
    `Kebakaran hutan dan lahan` = c("curah_hujan", "suhu_rata2", "kelembaban"),
    `Cuaca ekstrem` = c("kecepatan_angin", "tekanan_udara"),
    `Gelombang pasang / Abrasi` = c("kecepatan_angin", "tekanan_udara"),
    "Kekeringan" = c("curah_hujan", "suhu_rata2"),
    "Longsor" = c("curah_hujan", "kelembaban")
  )
  
  observe({
    dep <- input$dep_var
    if (!is.null(dep)) {
      default_indep <- default_x_map[[dep]]
      if (!is.null(default_indep)) {
        updateSelectizeInput(
          session, "indep_vars",
          selected = default_indep
        )
      }
    }
  })
  
  
  observeEvent(input$dep_var, {
    dep <- input$dep_var
    pilihan <- setdiff(numeric_vars, dep)
    pilihan_formatted <- format_label(pilihan)
    
    # Gunakan default dari map jika tersedia
    default_x <- default_x_map[[dep]]
    default_x <- default_x[default_x %in% pilihan]  # pastikan ada di data
    
    # Jika tidak ada default, ambil 3 pertama dari sisa variabel
    if (is.null(default_x) || length(default_x) == 0) {
      default_x <- pilihan[1:min(3, length(pilihan))]
    }
    
    updateSelectInput(session, "indep_vars",
                      choices  = setNames(pilihan, pilihan_formatted),
                      selected = default_x
    )
  }, ignoreInit = TRUE)
  
  model_fit <- reactive({
    req(input$dep_var, input$indep_vars)
    validate(need(length(input$indep_vars) >= 1, "Pilih minimal 1 variabel independen."))
    
    # Bungkus nama variabel dengan backtick agar aman jika ada spasi/garing bawah
    dep_bt   <- paste0("`", input$dep_var, "`")
    indep_bt <- paste0("`", input$indep_vars, "`")
    
    # Buat formula regresi yang valid
    formula_str <- paste(dep_bt, "~", paste(indep_bt, collapse = " + "))
    lm(as.formula(formula_str), data = data_dashboard)
  })
  
  output$model_summary <- renderPrint({
    summary(model_fit())
  })
  
  output$diagnostic_plot <- renderPlot({
    par(mfrow = c(2, 2))
    plot(model_fit())
    par(mfrow = c(1, 1))
  })
  
  output$asumption <- renderPrint({
    req(model_fit())
    
    model <- model_fit()
    
    # 1. UJI HOMOSKEDASTISITAS (Breusch-Pagan Test)
    cat("1. UJI HOMOSKEDASTISITAS\n")
    cat("   H₀: Varians error konstan (terdapat homoskedastis)\n")
    cat("   H₁: Varians error tidak konstan (terdapat heteroskedastis)\n")
    
    tryCatch({
      library(lmtest)
      bp_test <- bptest(model)
      cat("   Breusch-Pagan Test:\n")
      cat("   - Statistics BP =", round(bp_test$statistic, 4), "\n")
      cat("   - p-value =", round(bp_test$p.value, 4), "\n")
      if(bp_test$p.value > 0.05) {
        cat("   - Keputusan = GAGAL TOLAK H₀ (p > 0.05)\n\n")
        cat("   Berdasarkan tingkat signifikansi 5%, p-value uji Breusch-Pagan lebih besar dari α, sehingga H₀ diterima.\n")
        cat("   Dengan demikian, model regresi memenuhi asumsi homoskedastisitas atau variansi residual dianggap konstan.\n")
      } else {
        cat("   - Keputusan = TOLAK H₀ ASUMSI HOMOSKEDASTISITAS TIDAK TERPENUHI (p ≤ 0.05)\n\n")
        cat("   Berdasarkan tingkat signifikansi 5%, p-value uji Breusch-Pagan kurang dari atau sama dengan α, sehingga H₀ ditolak.\n")
        cat("   Dengan demikian, model regresi belum memenuhi asumsi homoskedastisitas atau terdapat heteroskedastisitas pada model.\n")
      }
    }, error = function(e) {
      cat("   Error dalam uji Breusch-Pagan:", e$message, "\n")
    })
    
    cat("\n", rep("-", 60), "\n\n")
    
    # 2. UJI NORMALITAS RESIDUAL
    cat("2. UJI NORMALITAS RESIDUAL\n")
    cat("   H₀: Residual berdistribusi normal\n")
    cat("   H₁: Residual tidak berdistribusi normal\n")
    
    residuals <- residuals(model)
    
    # Shapiro-Wilk Test (untuk n < 5000)
    if(length(residuals) < 5000) {
      sw_test <- shapiro.test(residuals)
      cat("   Shapiro-Wilk Test:\n")
      cat("   - Statistics SW =", round(sw_test$statistic, 4), "\n")
      cat("   - p-value =", round(sw_test$p.value, 4), "\n")
      if(sw_test$p.value > 0.05) {
        cat("   - Keputusan = GAGAL TOLAK H₀ (p > 0.05)\n\n")
        cat("   Berdasarkan tingkat signifikansi 5%, p-value uji Shapiro-Wilk lebih besar dari α, sehingga H₀ diterima.\n")
        cat("   Dengan demikian, model regresi memenuhi asumsi normalitas error atau residual berdistribusi normal.\n")
      } else {
        cat("   - Keputusan = TOLAK H₀ (p ≤ 0.05)\n\n")
        cat("   Berdasarkan tingkat signifikansi 5%, p-value uji Shapiro-Wilk kurang dari atau sama dengan α, sehingga H₀ ditolak.\n")
        cat("   Dengan demikian, model regresi belum memenuhi asumsi normalitas error atau residual tidak berdistribusi normal.\n")
      }
    } else {
      # Kolmogorov-Smirnov Test untuk sampel besar
      ks_test <- ks.test(residuals, "pnorm", mean(residuals), sd(residuals))
      cat("   Kolmogorov-Smirnov Test:\n")
      cat("   - Statistics KS =", round(ks_test$statistic, 4), "\n")
      cat("   - p-value =", round(ks_test$p.value, 4), "\n")
      if(ks_test$p.value > 0.05) {
        cat("   - Keputusan = GAGAL TOLAK H₀ (p > 0.05)\n\n")
        cat("   Berdasarkan tingkat signifikansi 5%, p-value uji Kolmogorov-Smirnov lebih besar dari α, sehingga H₀ diterima.\n")
        cat("   Dengan demikian, model regresi memenuhi asumsi normalitas error atau residual berdistribusi normal.\n")
      } else {
        cat("   - Keputusan = TOLAK H₀ (p ≤ 0.05)\n\n")
        cat("   Berdasarkan tingkat signifikansi 5%, p-value uji Kolmogorov-Smirnov kurang dari atau sama dengan α, sehingga H₀ ditolak.\n")
        cat("   Dengan demikian, model regresi belum memenuhi asumsi normalitas error atau residual tidak berdistribusi normal.\n")
      }
    }
    
    cat("\n" , rep("-", 60), "\n\n")
    
    # 3. UJI MULTIKOLINEARITAS (VIF)
    cat("3. UJI MULTIKOLINEARITAS\n")
    cat("   Variance Inflation Factor (VIF)\n")
    cat("   - VIF < 5: Tidak ada multikolinearitas\n")
    cat("   - VIF 5-10: Multikolinearitas sedang\n")
    cat("   - VIF > 10: Multikolinearitas tinggi\n\n")
    
    tryCatch({
      if(length(input$indep_vars) > 1) {
        library(car)
        vif_values <- vif(model)
        cat("   Nilai VIF:\n")
        for(i in 1:length(vif_values)) {
          var_name <- names(vif_values)[i]
          vif_val <- vif_values[i]
          status <- if(vif_val < 5) "BAIK" else if(vif_val <= 10) "SEDANG" else "TINGGI"
          cat("   -", var_name, ":", round(vif_val, 3), "(", status, ")\n")
        }
        
        max_vif <- max(vif_values)
        if(max_vif < 5) {
          cat("\n   Keputusan = TIDAK ADA MULTIKOLINEARITAS (semua VIF < 5)\n\n")
          cat("   Dengan demikian, model regresi memenuhi asumsi nonmultikolinearitas\n")
        } else if(max_vif <= 10) {
          cat("\n   Keputusan = TERDAPAT MULTIKOLINEARITAS SEDANG PADA MODEL (ada VIF 5-10)\n\n")
          cat("   Dengan demikian, model regresi belum memenuhi asumsi nonmultikolinearitas.\n")
        } else {
          cat("\n   Keputusan = TERDAPAT MULTIKOLINEARITAS TINGGI PADA MODEL (ada VIF > 10)\n\n")
          cat("   Dengan demikian, model regresi belum memenuhi asumsi nonmultikolinearitas.\n")
        }
      } else {
        cat("   Model regresi hanya memiliki satu variabel independen, sehingga uji multikolinearitas tidak diperlukan.\n")
      }
    }, error = function(e) {
      cat("   Error dalam uji VIF:", e$message, "\n")
    })
    
    cat("\n", rep("-", 60), "\n\n")
    
    # 4. UJI AUTOKORELASI (Durbin-Watson Test)
    cat("4. UJI AUTOKORELASI\n")
    cat("   H₀: Nonautokorelasi\n")
    cat("   H₁: Autokorelasi\n")
    
    tryCatch({
      library(lmtest)
      dw_test <- dwtest(model)
      cat("   Durbin-Watson Test:\n")
      cat("   - Statistik DW =", round(dw_test$statistic, 4), "\n")
      cat("   - p-value =", round(dw_test$p.value, 4), "\n\n")
      
      # Interpretasi nilai DW
      dw_val <- as.numeric(dw_test$statistic)
      if(dw_val < 1.5) {
        cat("   Indikasi: Autokorelasi positif (DW < 1.5)\n")
      } else if(dw_val > 2.5) {
        cat("   Indikasi: Autokorelasi negatif (DW > 2.5)\n")
      } else {
        cat("   Indikasi: Tidak ada autokorelasi yang kuat (1.5 ≤ DW ≤ 2.5)\n")
      }
      
      if(dw_test$p.value > 0.05) {
        cat("   Keputusan: ASUMSI TIDAK ADA AUTOKORELASI TERPENUHI (p > 0.05)\n\n")
        cat("   Berdasarkan tingkat signifikansi 5%, p-value uji Durbin-Watson lebih besar dari α, sehingga H₀ diterima.\n") 
        cat("   Dengan demikian, model regresi memenuhi asumsi nonautokorelasi atau tidak terdapat autokorelasi pada model.\n")
      } else {
        cat("   Keputusan: ADA AUTOKORELASI (p ≤ 0.05)\n\n")
        cat("   Berdasarkan tingkat signifikansi 5%, p-value uji Durbin-Watson kurang dari atau sama dengan α, sehingga H₀ ditolak.\n")
        cat("   Dengan demikian, model regresi belum memenuhi asumsi nonautokorelasi atau masih terdapat autokorelasi pada model.\n")
      }
    }, error = function(e) {
      cat("   Error dalam uji Durbin-Watson:", e$message, "\n")
    })
    
    cat("\n" , rep("-", 60), "\n\n")
  })
  
  # Output Tabel Dinamis
  provinsi_filter_stat <- reactive({
    if (input$provinsi_terpilih_stat == "Indonesia"){
      data_dashboard[-1] %>% 
        filter(tahun >= input$rentang_tahun_provinsi_stat[1] & tahun <= input$rentang_tahun_provinsi_stat[2]) %>% 
        arrange(desc(tahun)) %>% 
        rename(
          Provinsi = provinsi,
          Tahun = tahun,
          `Korban banjir` = korban_Banjir,
          `Korban cuaca ekstrem` = korban_Cuaca_ekstrem,
          `Total korban` = Total_Korban,
          `Total kejadian` = Total_Kejadian,
          `Korban gelombang pasang / Abrasi` = korban_Gelombang_pasang_Abrasi,
          `Korban kebakaran hutan dan lahan` = korban_Kebakaran_hutan_dan_lahan,
          `Korban kekeringan` = korban_Kekeringan,
          `Korban Longsor` = korban_Longsor,
          `Suhu Rata-Rata` = suhu_rata2,
          `Curah hujan` = curah_hujan,
          `Kecepatan angin` = kecepatan_angin,
          Kelembaban = kelembaban,
          `Tekanan udara` = tekanan_udara
        )
    } else{
      data_dashboard[-1] %>%
        filter(provinsi == input$provinsi_terpilih_stat) %>% 
        filter(tahun >= input$rentang_tahun_provinsi_stat[1] & tahun <= input$rentang_tahun_provinsi_stat[2]) %>% 
        arrange(desc(tahun)) %>% 
        rename(
          Provinsi = provinsi,
          Tahun = tahun,
          `Korban banjir` = korban_Banjir,
          `Korban cuaca ekstrem` = korban_Cuaca_ekstrem,
          `Total korban` = Total_Korban,
          `Total kejadian` = Total_Kejadian,
          `Korban gelombang pasang / Abrasi` = korban_Gelombang_pasang_Abrasi,
          `Korban kebakaran hutan dan lahan` = korban_Kebakaran_hutan_dan_lahan,
          `Korban kekeringan` = korban_Kekeringan,
          `Korban Longsor` = korban_Longsor,
          `Suhu Rata-Rata` = suhu_rata2,
          `Curah hujan` = curah_hujan,
          `Kecepatan angin` = kecepatan_angin,
          Kelembaban = kelembaban,
          `Tekanan udara` = tekanan_udara
        ) 
    }
  })
  
  ## Tabel Data
  output$tabel_stat_provinsi <- DT::renderDataTable({
    DT::datatable(
      provinsi_filter_stat(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      caption = "Data Iklim dan Bencana per Wilayah"
    )
  })
  
  
  output$download_data_provinsi <- downloadHandler(
    filename = function() {
      paste0("data-statistik-", Sys.Date(), ".csv")
    },
    content = function(file) {
      data_lengkap <- provinsi_filter_stat()
      
      data_untuk_diunduh <- data_lengkap[input$tabel_stat_wilayah_rows_all, ]
      
      write.csv(data_untuk_diunduh, file, row.names = FALSE)
    }
  )
  
  data_stat_bencana <- reactive({
    req(input$bencana_terpilih_stat)
    
    peta_kejadian <- list(
      "Banjir" = "Banjir", "Cuaca ekstrem" = "Cuaca ekstrem", 
      "Gelombang pasang / Abrasi" = "Gelombang pasang / Abrasi",
      "Kebakaran hutan dan lahan" = "Kebakaran hutan dan lahan",
      "Kekeringan" = "Kekeringan", "Longsor" = "Longsor"
    )
    
    peta_korban <- list(
      "Banjir" = "korban_Banjir", "Cuaca ekstrem" = "korban_Cuaca_ekstrem",
      "Gelombang pasang / Abrasi" = "korban_Gelombang_pasang_Abrasi",
      "Kebakaran hutan dan lahan" = "korban_Kebakaran_hutan_dan_lahan",
      "Kekeringan" = "korban_Kekeringan", "Longsor" = "korban_Longsor"
    )
    
    bencana_dipilih <- input$bencana_terpilih_stat
    kolom_kejadian <- peta_kejadian[[bencana_dipilih]]
    kolom_korban <- peta_korban[[bencana_dipilih]]
    
    data_dashboard %>%
      select(provinsi, tahun, all_of(kolom_kejadian), all_of(kolom_korban)) %>%
      rename(
        Jumlah_Kejadian = all_of(kolom_kejadian),
        Jumlah_Korban_Jiwa = all_of(kolom_korban)
      ) %>%
      arrange(desc(tahun))
  })
  
  ## Render Tabel Data Bencana
  output$tabel_stat_bencana <- DT::renderDataTable({
    DT::datatable(
      data_stat_bencana(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      caption = paste("Tabel Rincian untuk Bencana:", input$bencana_terpilih_stat)
    )
  })
  
  ## Fungsi Tombol Download Bencana
  output$download_data_bencana <- downloadHandler(
    filename = function() {
      paste0("data-rincian-", gsub(" ","-", input$bencana_terpilih_stat), ".csv")
    },
    content = function(file) {
      data_lengkap <- data_stat_bencana()
      
      data_untuk_diunduh <- data_lengkap[input$tabel_stat_bencana_rows_all, ]
      
      write.csv(data_untuk_diunduh, file, row.names = FALSE)
    }
  )
  
  observeEvent(input$to_dashboard_button,{
    updateTabItems(
      session = session,
      inputId = "sidebarMenuid",
      selected = "dashboard-provinsi"
    )
  })
  
  rv <- reactiveValues(show_more = FALSE)
  
  observeEvent(input$more_info_button, {
    rv$show_more <- !rv$show_more
  })
  
  output$more_info_ui <- renderUI({
    if (rv$show_more) {
      return(
        div(
          style = "text-align: left;",
          tagList(
            h4("Latar Belakang", style = "font-weight: bold;"),
            p("Perubahan iklim berdampak besar terhadap peningkatan frekuensi dan intensitas bencana hidrometeorologi di Indonesia, seperti banjir, kekeringan, dan angin kencang. Fenomena ini bukan lagi kejadian luar biasa, melainkan pola baru yang makin kompleks dan meluas dampaknya."),
            p("Statistik lingkungan berperan penting dalam menyusun kebijakan adaptasi dan mitigasi. Indonesia, sebagai negara kepulauan, sangat rentan terhadap krisis iklim yang mengancam aspek sosial, ekonomi, hingga kesehatan. BNPB mencatat tingginya kejadian bencana pada 2020–2024, menandakan urgensi akses data yang cepat dan terintegrasi."),
            p("Dashboard ini hadir untuk menjawab kebutuhan tersebut, menyajikan data korban bencana terkait perubahan iklim secara interaktif agar dapat digunakan sebagai dasar pengambilan keputusan yang tepat dan berbasis bukti."),
            
            h4("Rumusan Masalah", style = "font-weight: bold;"),
            tags$ul(
              tags$li("Bagaimana dampak perubahan iklim terhadap peningkatan frekuensi dan intensitas bencana hidrometeorologi di Indonesia pada 2020–2024?"),
              tags$li("Apa saja faktor yang menyebabkan kerentanan masyarakat terhadap bencana yang dipengaruhi oleh perubahan iklim?"),
              tags$li("Apa tantangan dalam pemantauan dan analisis data korban bencana akibat perubahan iklim, dan bagaimana mengatasinya?")
            ),
            
            h4("Tujuan", style = "font-weight: bold;"),
            tags$ul(
              tags$li("Menganalisis dampak perubahan iklim terhadap bencana di Indonesia selama 2020–2024."),
              tags$li("Mengidentifikasi faktor kerentanan masyarakat terhadap dampak cuaca ekstrem."),
              tags$li("Merumuskan pendekatan penyajian data yang lebih efektif untuk mendukung kebijakan berbasis data.")
            )
          )
        )
      )
    } else {
      return(NULL)
    }
  })
  
  # Kontrol tampilan halaman profile
  profilePage <- reactiveVal("overview")
  
  # Ubah halaman saat tombol diklik
  observeEvent(input$detail_tm, { profilePage("1") })
  observeEvent(input$detail_qurany, { profilePage("2") })
  observeEvent(input$detail_wahyu, { profilePage("3") })
  observeEvent(input$detail_dinda, { profilePage("4") })
  observeEvent(input$detail_hamlul, { profilePage("5") })
  observeEvent(input$back, { profilePage("overview") })
  
  output$profileUI <- renderUI({
    if (profilePage() == "overview") {
      div(class = "profile-content",
          fluidRow(
            column(width = 4,
                   div(class = "profile-card",
                       div(style = "text-align: center;",
                           tags$img(src = "profile_1.png",
                                    width = "100%",
                                    style = "margin-bottom: 15px;"),
                           h4("T.M. Al-Asy'ari Al-Muchtari"),
                           p("Mahasiswa"),
                           actionButton("detail_tm", "View Detail", class = "btn-profile")
                       )
                   )
            ),
            column(width = 4,
                   div(class = "profile-card",
                       div(style = "text-align: center;",
                           tags$img(src = "profile_2.png",
                                    width = "100%",
                                    style = "margin-bottom: 15px;"),
                           h4("Qurany Nadhira Tsabita"),
                           p("Mahasiswa"),
                           actionButton("detail_qurany", "View Detail", class = "btn-profile")
                       )
                   )
            ),
            column(width = 4,
                   div(class = "profile-card",
                       div(style = "text-align: center;",
                           tags$img(src = "profile_3.png",
                                    width = "100%",
                                    style = "margin-bottom: 15px;"),
                           h4("Wahyu Nugraha Raomi Gading"),
                           p("Mahasiswa"),
                           actionButton("detail_wahyu", "View Detail", class = "btn-profile")
                       )
                   )
            )
          ),
          fluidRow(
            column(width = 4, offset = 2,
                   div(class = "profile-card",
                       div(style = "text-align: center;",
                           tags$img(src = "profile_4.png",
                                    width = "100%",
                                    style = "margin-bottom: 15px;"),
                           h4("Dinda Putri Nur Wulandari"),
                           p("Mahasiswa"),
                           actionButton("detail_dinda", "View Detail", class = "btn-profile")
                       )
                   )
            ),
            column(width = 4,
                   div(class = "profile-card",
                       div(style = "text-align: center;",
                           tags$img(src = "profile_5.png",
                                    width = "100%",
                                    style = "margin-bottom: 15px;"),
                           h4("Muhammad Hamlul Khair"),
                           p("Mahasiswa"),
                           actionButton("detail_hamlul", "View Detail", class = "btn-profile")
                       )
                   )
            )
          )
      )
    } else {
      detail <- switch(profilePage(),
                       "1" = list(
                         name = "T.M. Al-Asy'ari Al-Muchtari",
                         img = "profile_1.png",
                         email = "222313397@stis.ac.id",
                         asal = "Bireuen, Aceh",
                         kontribusi = "test"
                       ),
                       "2" = list(   
                         name = "Qurany Nadhira Tsabita", 
                         img = "profile_2.png", 
                         email = "222313323@stis.ac.id",
                         asal = "Kediri, Jawa Timur",
                         kontribusi = "test"
                       ),
                       "3" = list(
                         name = "Wahyu Nugraha Raomi Gading",
                         img = "profile_3.png",
                         email = "222313421@stis.ac.id", 
                         asal = "Sleman, DI Yogyakarta",
                         kontribusi = "test"
                       ),
                       "4" = list(
                         name = "Dinda Putri Nur Wulandari", 
                         img = "profile_4.png",
                         email = "222313054@stis.ac.id", 
                         asal = "Bandung Barat, Jawa Barat", 
                         kontribusi = "test"
                       ),
                       "5" = list(
                         name = "Muhammad Hamlul Khair", 
                         img = "profile_5.png",
                         email = "222313241@stis.ac.id", 
                         asal = "Aceh", 
                         kontribusi = "test"
                       ))
      
      div(class = "profile-content",
          div(class = "profile-card detail-panel",
              fluidRow(
                column(width = 4,
                       div(class = "detail-image-container",
                           tags$img(src = detail$img,
                                    class = "detail-image",
                                    width = "100%")
                       )
                ),
                column(width = 8,
                       div(class = "detail-info",
                           h3(detail$name, class = "detail-name"),
                           div(class = "detail-item",
                               p(tags$strong("Email "), detail$email)
                           ),
                           div(class = "detail-item", 
                               p(tags$strong("Asal Daerah "), detail$asal)
                           ),
                           div(class = "detail-item",
                               p(tags$strong("Kontribusi "), detail$kontribusi)
                           ),
                           br(),
                           actionButton("back", "Back", class = "back-btn")
                       )
                )
              )
          )
      )
    }
  })
}

shinyApp(ui, server)