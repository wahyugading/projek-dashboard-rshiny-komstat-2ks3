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
library(sf)
library(leaflet)
library(car)
library(lmtest)
library(here)
library(stringr)
library(forecast)
library(scales)
library(shinyWidgets)
library(fontawesome)
library(rmarkdown)
library(knitr)

data_dashboard <-  read_excel("Dashboard Data.xlsx")
peta_indonesia_38provinsi <- st_read("38 Provinsi Indonesia - Provinsi.json") 
peta_indonesia_37provinsi <- st_read("37 Provinsi Indonesia - Provinsi.geojson") 
peta_indonesia_34provinsi <- st_read("34 Provinsi Indonesia - Provinsi.geojson") 

peta_indonesia_37provinsi <-  peta_indonesia_37provinsi[c(-3,-4)] %>% 
  rename( 
    PROVINSI = name 
  ) 

peta_indonesia_37provinsi <-  peta_indonesia_37provinsi %>%
  mutate(
    PROVINSI = case_when(
      PROVINSI == "PAPUA PEGUNUNGAN TENGAH" ~ "Papua Pegunungan",
      TRUE ~ str_to_title(PROVINSI)
    )
  ) 

peta_indonesia_34provinsi <- peta_indonesia_34provinsi %>% 
  rename( 
    PROVINSI = name 
  ) 

peta_indonesia_34provinsi <- peta_indonesia_34provinsi %>%
  mutate(
    PROVINSI = case_when(
      TRUE ~ str_to_title(PROVINSI)
    )
  )

variabel_iklim <- c("Suhu Rata-Rata" = "suhu_rata2",
                    "Curah Hujan" = "curah_hujan",
                    "Kelembaban" = "kelembaban", 
                    "Kecepatan Angin" = "kecepatan_angin",
                    "Tekanan Udara" = "tekanan_udara")

numeric_vars <- data_dashboard %>%
  select(where(is.numeric), -tahun) %>% # Pilih semua kolom numerik, KECUALI kolom 'tahun'
  names()

format_label <- function(x) {
  sapply(x, function(n) {
    n_clean <- gsub("_", " ", n)
    words <- strsplit(n_clean, " ")[[1]]
    paste(toupper(substring(words, 1,1)), tolower(substring(words, 2)), sep="", collapse=" ")
  }, USE.NAMES = FALSE)
}

# KAMUS DATA UNTUK METADATA
metadata_kamus <- list(
  "Total_Kejadian" = list(
    `Nama Variabel` = "Total Kejadian",
    Konsep = "Jumlah Kejadian Bencana",
    Definisi = "Banyaknya kejadian peristiwa atau rangkaian peristiwa yang mengancam dan mengganggu kehidupan dan penghidupan masyarakat yang disebabkan faktor alam dan/atau faktor nonalam maupun faktor manusia, sehingga mengakibatkan timbulnya korban manusia, kerusakan lingkungan, kerugian harta benda, dan dampak psikologis.",
    `Referensi Waktu` = "Januari - Desember",
    Ukuran = "Jumlah kejadian bencana",
    Satuan = "Kejadian",
    `Tipe Data` = "Integer"
  ),
  "Total_Korban" = list(
    `Nama Variabel` = "Total Korban",
    Konsep = "Korban Bencana",
    Definisi = "Banyaknya orang yang mengalami kerusakan lingkungan, kerugian harta benda, dampak psikologis, dan meninggal dunia akibat kecelakaan, bencana, dan/atau kondisi membahayakan manusia.",
    `Referensi Waktu` = "Januari - Desember",
    Ukuran = "Jumlah",
    Satuan = "Jiwa",
    `Tipe Data` = "Integer"
  ),
  "Banjir" = list(
    `Nama Variabel` = "Banjir",
    Konsep = "Kejadian Bencana Banjir",
    Definisi = "Banjir merupakan peristiwa terendamnya suatu wilayah secara tiba-tiba karena jumlah debit air yang besar akibat terbendungnya aliran sungai. Banjir dapat terjadi karena curah hujan yang sangat tinggi namun tidak diimbangi dengan adanya saluran pembuangan air yang memadai. Banjir rob terjadi akibat adanya kenaikan muka air laut yang disebabkan oleh pasang surut air laut.",
    `Referensi Waktu` = "Januari - Desember",
    Ukuran = "Jumlah kejadian bencana banjir",
    Satuan = "Kejadian",
    `Tipe Data` = "Integer"
  ),
  "Cuaca ekstrem" = list(
    `Nama Variabel` = "Cuaca Ekstrem",
    Konsep = "Kejadian Bencana Cuaca Ekstrem",
    Definisi = "Jumlah kejadian bencana cuaca ekstrem selama satu tahun.",
    `Referensi Waktu` = "Januari - Desember",
    Ukuran = "Jumlah kejadian bencana cuaca ekstrem",
    Satuan = "Kejadian",
    `Tipe Data` = "Integer"
  ),
  "Gelombang pasang / Abrasi" = list(
    `Nama Variabel` = "Gelombang Pasang / Abrasi",
    Konsep = "Kejadian bencana Gelombang Pasang/Abrasi",
    Definisi = "Bencana Gelombang Pasang/Abrasi yang menimbulkan dampak kerugian material atau korban jiwa di suatu wilayah Indonesia satu tahun.",
    `Referensi Waktu` = "Januari - Desember",
    Ukuran = "Jumlah kejadian bencana gelombang pasang/abrasi",
    Satuan = "Kejadian",
    `Tipe Data` = "Integer"
  ),
  "Kebakaran hutan dan lahan" = list(
    `Nama Variabel` = "Kebakaran Hutan dan Lahan",
    Konsep = "Kejadian Bencana Kebakaran Hutan dan Lahan",
    Definisi = "Suatu keadaan di mana hutan dan lahan dilanda api, sehingga mengakibatkan kerusakan hutan dan lahan yang menimbulkan kerugian ekonomis dan atau nilai lingkungan.",
    `Referensi Waktu` = "Januari - Desember",
    Ukuran = "Total",
    Satuan = "Titik",
    `Tipe Data` = "Integer"
  ),
  "Kekeringan" = list(
    `Nama Variabel` = "Kekeringan",
    Konsep = "Kejadian Bencana Kekeringan",
    Definisi = "Kondisi di mana suatu wilayah mengalami defisit curah hujan yang berkepanjangan sehingga mengakibatkan kekurangan air untuk kebutuhan rumah tangga, pertanian, dan aktivitas lainnya.",
    `Referensi Waktu` = "Januari - Desember",
    Ukuran = "Jumlah kejadian bencana kekeringan",
    Satuan = "Unit",
    `Tipe Data` = "Numerik"
  ),
  "Longsor" = list(
    `Nama Variabel` = "Longsor",
    Konsep = "Jumlah bencana yang terjadi akibat Tanah Longsor",
    Definisi = "Salah satu jenis gerakan massa tanah atau batuan, ataupun percampuran keduanya, menuruni atau keluar lereng akibat terganggunya kestabilan tanah atau batuan penyusun lereng.",
    `Referensi Waktu` = "Januari - Desember",
    Ukuran = "Jumlah kejadian bencana longsor",
    Satuan = "Kejadian",
    `Tipe Data` = "Integer"
  ),
  "suhu_rata2" = list(
    `Nama Variabel` = "Suhu Rata-Rata",
    Konsep = "Suhu",
    Definisi = "Keadaan Panas Atau Dinginnya Suatu Tempat Pada Waktu Tertentu Yang Dapat Mempengaruhi Besarnya Parameter Lain Seperti Kelembaban Udara.",
    `Referensi Waktu` = "Januari - Desember",
    Ukuran = "Rata-Rata",
    Satuan = "Celcius",
    `Tipe Data` = "Float"
  ),
  "curah_hujan" = list(
    `Nama Variabel` = "Curah Hujan",
    Konsep = "Jumlah curah hujan per tahun",
    Definisi = "Curah hujan adalah ketinggian air hujan yang terkumpul dalam penakar hujan pada tempat yang datar, tidak menyerap, tidak meresap dan tidak mengalir. Tinggi air yang jatuh ini biasanya dinyatakan dengan satuan milimeter di suatu wilayah Indonesia. Curah hujan dalam 1 (satu) millimeter artinya dalam luasan satu meter persegi, tempat yang datar dapat menampung air hujan setinggi satu mm atau sebanyak satu liter.",
    `Referensi Waktu` = "Januari - Desember",
    Ukuran = "Total",
    Satuan = "mm/tahun",
    `Tipe Data` = "Float"
  ),
  "kecepatan_angin" = list(
    `Nama Variabel` = "Kecepatan Angin",
    Konsep = "Kecepatan angin diukur dengan menggunakan anemometer atau dapat diklasifikasikan dengan menggunakan skala Beaufort yang didasarkan pada pengamatan pengaruh spesifik dari kecepatan angin tertentu.",
    Definisi = "Satuan yang mengukur kecepatan aliran udara dari tekanan tinggi ke tekanan rendah.",
    `Referensi Waktu` = "Januari - Desember",
    Ukuran = "Rata-Rata",
    Satuan = "Knot",
    `Tipe Data` = "Float"
  ),
  "kelembaban" = list(
    `Nama Variabel` = "Kelembaban",
    Konsep = "Kelembaban Udara",
    Definisi = "Banyaknya Uap Air Yang Terkandung Dalam Udara Atau Atmosfer Atau Dapat Pula Diartikan Sebagai Jumlah Kadar Uap Air Yang Ada Dalam Udara.",
    `Referensi Waktu` = "Setiap hari, sebanyak 365 hari dalam setahun",
    Ukuran = "Rata-Rata",
    Satuan = "Persen",
    `Tipe Data` = "Float"
  ),
  "tekanan_udara" = list(
    `Nama Variabel` = "Tekanan Udara",
    Konsep = "Tekanan Udara",
    Definisi = "Gaya Per Satuan Luas Yang Disebabkan Oleh Berat Udara Di Atasnya.",
    `Referensi Waktu` = "Januari - Desember",
    Ukuran = "Rata-Rata",
    Satuan = "Centibar",
    `Tipe Data` = "Float"
  )
)

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
          tabName = "dashboard-provinsi"
        ),
        menuSubItem(
          "Menurut Bencana Alam",
          tabName = "dashboard-bencana"
        )
      ),
      
      menuItem(
        "Peta",
        tabName = "peta",
        icon = icon("map"),
        menuSubItem(
          "Peta Bencana",
          tabName = "peta-bencana",
        ),
        menuSubItem(
          "Peta Iklim",
          tabName = "peta-iklim"
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
        "Metadata",
        tabName = "metadata",
        icon = icon("database")
      ),
      
      menuItem(
        "Profile Tim",
        tabName = "profile",
        icon = icon("user")
      )
    )
  ),
  footer = dashboardFooter(
    left = "Dashboard Kelompok 6 Kelas 2KS3",
    right = format(Sys.Date(), "V1.0.0 Copyright Politeknik Statistika STIS %Y")
  ),
  body = dashboardBody(
    tags$head(
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
          
          column(width = 12,
                 box(
                   title = tags$strong("Petunjuk Penggunaan Halaman"),
                   status = "info",
                   width = 12,
                   icon = icon("book-open"),
                   p("Halaman ini menampilkan ringkasan data iklim dan bencana untuk wilayah yang Anda pilih. Ikuti langkah-langkah berikut:"),
                   tags$ol(
                     tags$li(tags$strong("Pilih Provinsi:"), " Gunakan dropdown 'Pilih Provinsi' untuk melihat data spesifik suatu provinsi, atau pilih 'Indonesia' untuk melihat data agregat nasional."),
                     tags$li(tags$strong("Atur Rentang Tahun:"), " Geser slider 'Pilih Rentang Tahun' untuk memfokuskan analisis pada periode waktu yang Anda inginkan."),
                     tags$li(tags$strong("Analisis Visual:"), " Amati perubahan pada Info Box, grafik tren, dan diagram proporsi yang akan diperbarui secara otomatis sesuai pilihan Anda.")
                   )
                 )
          )
        ),
        box(width=12,
            title=strong("Dashboard Menurut Provinsi"),
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "provinsi_terpilih",
                  label = "Pilih Provinsi :",
                  choices  = unique(data_dashboard$provinsi),
                  selected = "Aceh",
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
                height = "525px",
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
                height = "525px",
                plotlyOutput("top_bencana_plot", height = "450px")
              )
            ),
            
            fluidRow(
              box(
                title = "Proporsi Sebaran Kejadian Bencana Alam",
                status = "success",
                solidHeader = TRUE,
                width = 6,
                height = "500px",
                plotlyOutput("pie_bencana_plot",height = "450px")
              ),
              box(
                title = "Proporsi Sebaran Korban Bencana Alam",
                status = "success",
                solidHeader = TRUE,
                width = 6,
                height = "500px",
                plotlyOutput("pie_korban_plot",height = "450px")
              ),
            ),
            # FITUR UNDUH LAPORAN DASHBOARD PROVINSI
            fluidRow(
              box(
                title = "Unduh Laporan",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                div(
                  style = "text-align: center; padding: 20px;",
                  p("Unduh laporan lengkap analisis dashboard menurut provinsi dalam format PDF"),
                  downloadButton("download_laporan_provinsi", 
                                 "Unduh Laporan Dashboard Provinsi", 
                                 class = "btn-primary btn-lg",
                                 style = "margin: 10px;")
                )
              )
            )
        )
      ),
      
      tabItem(
        tabName = "dashboard-bencana",
        fluidRow(
          
          column(width = 12,
                 box(
                   title = tags$strong("Petunjuk Penggunaan Halaman"),
                   status = "info",
                   width = 12,
                   icon = icon("book-open"),
                   p("Halaman ini menampilkan ringkasan data jenis bencana yang Anda pilih. Ikuti langkah-langkah berikut:"),
                   tags$ol(
                     tags$li(tags$strong("Pilih Bencana:"), " Gunakan dropdown 'Pilih Bencana' untuk melihat data spesifik suatu jenis bencana."),
                     tags$li(tags$strong("Atur Rentang Tahun:"), " Geser slider 'Pilih Rentang Tahun' untuk memfokuskan analisis pada periode waktu yang Anda inginkan."),
                     tags$li(tags$strong("Analisis Visual:"), " Amati perubahan pada Info Box, grafik tren, dan diagram batang yang akan diperbarui secara otomatis sesuai pilihan Anda.")
                   )
                 )
          )
        ),
        box(width=12,
            title = strong("Dashboard Menurut Jenis Bencana"),
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
                uiOutput("top_provinsi_box")
              )
            ),
            # FITUR UNDUH LAPORAN DASHBOARD BENCANA
            fluidRow(
              box(
                title = "Unduh Laporan",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                div(
                  style = "text-align: center; padding: 20px;",
                  p("Unduh laporan lengkap analisis dashboard menurut jenis bencana dalam format PDF"),
                  downloadButton("download_laporan_bencana", 
                                 "Unduh Laporan Dashboard Bencana", 
                                 class = "btn-primary btn-lg",
                                 style = "margin: 10px;")
                )
              )
            )
        )
      ),
      
      tabItem(
        tabName = "peta-bencana",
        fluidRow(
          
          column(width = 12,
                 box(
                   title = tags$strong("Petunjuk Penggunaan Halaman"),
                   status = "info",
                   width = 12,
                   icon = icon("book-open"),
                   p("Halaman ini menampilkan peta sebaran bencana yang Anda pilih di tiap provinsi Indonesia. Ikuti langkah-langkah berikut:"),
                   tags$ol(
                     tags$li(tags$strong("Pilih Bencana:"), " Gunakan dropdown 'Pilih Bencana' untuk melihat data spesifik suatu jenis bencana."),
                     tags$li(tags$strong("Pilih Tahun:"), " Gunakan dropdown 'Pilih Tahun' untuk memfokuskan analisis pada periode waktu yang Anda inginkan."),
                     tags$li(tags$strong("Analisis Visual:"), " Amati sebaran data melalui peta cloropeth sesuai pilihan Anda tadi.")
                   )
                 )
          )
        ),
        box(width=12,
            title =strong ("Peta Sebaran Bencana di Indonesia"),
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "peta_bencana_terpilih",
                  label = "Pilih Bencana :",
                  choices = c("Banjir", "Cuaca ekstrem", "Gelombang pasang / Abrasi", "Kebakaran hutan dan lahan", "Kekeringan", "Longsor"),
                  selected = "Banjir",
                  width = "100%"
                )
              ),
              
              column(
                width = 4,
                selectInput(
                  inputId = "tahun_peta_bencana",
                  label = "Pilih Tahun :",
                  choices = unique(data_dashboard$tahun),
                  selected = 2024,
                  width = "100%"
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                uiOutput("peta_bencana_box")
              )
            ),
            # FITUR UNDUH LAPORAN PETA BENCANA
            fluidRow(
              box(
                title = "Unduh Laporan",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                div(
                  style = "text-align: center; padding: 20px;",
                  p("Unduh laporan lengkap analisis peta sebaran bencana dalam format PDF"),
                  downloadButton("download_laporan_peta_bencana", 
                                 "Unduh Laporan Peta Bencana", 
                                 class = "btn-primary btn-lg",
                                 style = "margin: 10px;")
                )
              )
            )
        )
      ),
      
      tabItem(
        tabName = "peta-iklim",
        fluidRow(
          column(width = 12,
                 box(
                   title = tags$strong("Petunjuk Penggunaan Halaman"),
                   status = "info",
                   width = 12,
                   icon = icon("book-open"),
                   p("Halaman ini menampilkan peta sebaran perubahan iklim yang Anda pilih di tiap provinsi Indonesia. Ikuti langkah-langkah berikut:"),
                   tags$ol(
                     tags$li(tags$strong("Pilih Variabel Iklim:"), " Gunakan dropdown 'Pilih Variabel Iklim' untuk melihat data spesifik suatu jenis variabel iklim"),
                     tags$li(tags$strong("Pilih Tahun:"), " Gunakan dropdown 'Pilih Tahun' untuk memfokuskan analisis pada periode waktu yang Anda inginkan."),
                     tags$li(tags$strong("Analisis Visual:"), " Amati sebaran data melalui peta cloropeth sesuai pilihan Anda tadi.")
                   )
                 )
          )
        ),
        box(width=12,
            title=strong("Peta Sebaran Perubahan Iklim di Indonesia"),
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "peta_iklim_terpilih",
                  label = "Pilih Variabel Iklim : ",
                  choices = variabel_iklim,
                  selected = "Suhu Rata-Rata",
                  width = "100%"
                ),
              ),
              
              column(
                width = 4,
                selectInput(
                  inputId = "tahun_peta_iklim",
                  label = "Pilih Tahun :",
                  choices = unique(data_dashboard$tahun),
                  selected = 2024,
                  width = "100%"
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                uiOutput("peta_iklim_box")
              )
            ),
            # FITUR UNDUH LAPORAN PETA IKLIM
            fluidRow(
              box(
                title = "Unduh Laporan",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                div(
                  style = "text-align: center; padding: 20px;",
                  p("Unduh laporan lengkap analisis peta sebaran iklim dalam format PDF"),
                  downloadButton("download_laporan_peta_iklim", 
                                 "Unduh Laporan Peta Iklim", 
                                 class = "btn-primary btn-lg",
                                 style = "margin: 10px;")
                )
              )
            )
        )
      ),
      
      tabItem(
        tabName = "analisis-korelasi",
        fluidRow(
          column(width = 12,
                 box(
                   title = tags$strong("Petunjuk Penggunaan Halaman"),
                   status = "info",
                   width = 12,
                   icon = icon("book-open"),
                   p("Halaman ini menampilkan analisis korelasi hubungan antar variabel data tiap provinsi di Indonesia. Ikuti langkah-langkah berikut:"),
                   tags$ol(
                     tags$li(tags$strong("Pilih Provinsi:"), " Gunakan dropdown 'Pilih Provinsi' untuk melakukan analisis data spesifik suatu provinsi"),
                     tags$li(tags$strong("Pilih Variabel:"), " Gunakan dropdown 'Pilih Variabel' untuk memasukkan variabel tambahan yang Anda ingin uji korelasinya."),
                     tags$li(tags$strong("Analisis Statistik:"), " Amati visual dalam bentuk scatter-matrix dan hasil uji statistik tabel korelasi, serta interpretasi di provinsi dan variabel yang Anda pilih.")
                   )
                 )
          )
        ),
        box(width=12,
            title=strong("Analisis Korelasi Antar Variabel"),
            
            fluidRow(
              column(
                width = 4,
                
                # Box Pilih Provinsi
                box(
                  width = NULL, height = "125px", status = "primary", solidHeader = TRUE,
                  title = "Pilih Provinsi",
                  selectInput(
                    inputId  = "prov_korelasi",
                    label    = "Provinsi:",
                    choices  = unique(data_dashboard$provinsi),
                    selected = "Aceh"
                  )
                ),
                
                # Box Pilih Variabel
                box(
                  width = NULL, height = "210px", status = "primary", solidHeader = TRUE,
                  title = "Pilih Variabel",
                  selectizeInput(
                    inputId  = "vars_korelasi",
                    label    = "Variabel Numerik (≥2):",
                    choices  = setNames(numeric_vars, format_label(numeric_vars)),
                    selected = numeric_vars[1:2],
                    multiple = TRUE,
                    options  = list(
                      plugins = list("remove_button"),
                      maxItems = NULL,
                      dropdownMaxHeight = "300px"
                    )
                  )
                )
              ),
              
              column(
                width = 8,
                
                # Scatter Matrix
                box(
                  width = NULL, height = "500px", status = "info", solidHeader = TRUE,
                  title = "Scatter‑Matrix",
                  plotOutput("scatterplot", height = "440px")
                )
              )
            ),
            
            fluidRow(
              # Matriks Korelasi
              box(
                width = 12, status = "success", solidHeader = TRUE,
                title = "Matriks Korelasi",
                DTOutput("cor_matrix")
              )
            ),
            fluidRow(
              # Interpretasi Korelasi
              box(
                width = 12, status = "info", solidHeader = TRUE,
                title = "Interpretasi Korelasi",
                verbatimTextOutput("interpretasi_korelasi")
              )
            ),
            # FITUR UNDUH LAPORAN ANALISIS KORELASI
            fluidRow(
              box(
                title = "Unduh Laporan",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                div(
                  style = "text-align: center; padding: 20px;",
                  p("Unduh laporan lengkap analisis korelasi dalam format PDF"),
                  downloadButton("download_laporan_korelasi", 
                                 "Unduh Laporan Analisis Korelasi", 
                                 class = "btn-primary btn-lg",
                                 style = "margin: 10px;")
                )
              )
            )
        )
      ),
      
      tabItem(
        tabName = "analisis-regresi",
        fluidRow(
          column(width = 12,
                 box(
                   title = tags$strong("Petunjuk Penggunaan Halaman"),
                   status = "info",
                   width = 12,
                   icon = icon("book-open"),
                   p("Halaman ini menampilkan analisis korelasi hubungan antar variabel data tiap provinsi di Indonesia. Ikuti langkah-langkah berikut:"),
                   tags$ol(
                     tags$li(tags$strong("Pilih Provinsi:"), " Gunakan dropdown 'Pilih Provinsi' untuk melakukan analisis data spesifik suatu provinsi"),
                     tags$li(tags$strong("Pilih Variabel Dependen (Y):"), " Gunakan dropdown 'Pilih Variabel Dependen' untuk memasukkan variabel terikat (Y) yang ingin diteliti."),
                     tags$li(tags$strong("Pilih Variabel Independen (X):"), " Gunakan dropdown 'Pilih Variabel Independen' untuk memasukkan variabel bebas lain (X) yang ingin diuji signifikansi terhadap variabel terikat yang akan diteliti."),
                     tags$li(tags$strong("Pilih Transformasi Variabel:"), " Gunakan radio button 'Transformasi Y' untuk memilih transformasi data variabel terikat (Y) dan 'Transformasi X' untuk memilih transformasi data variabel bebas (X) untuk melihat analisis statistik dengan metode berbeda."),
                     tags$li(tags$strong("Analisis Statistik:"), " Lihat ringkasan model regresi untuk melihat model regresi yang terbentuk dari variabel bebas dan terikat yang sudah dipilih.
                             Lihat statistik deskriptif untuk melihat ukuran pemusatan data.
                             Lihat diagnostik plot untuk melihat uji kenormalan data lewat visual."),
                     tags$li(tags$strong("Ringkasan Pemeriksaan Uji Model Regresi:"), " Lihat pemeriksaan asumsi klasik, yaitu normalitas, homoskedastisitas, non-autokorelasi, dan non-multikolinieritas.
                             Jika ada asumsi yang terlanggar taksiran parameter masih dapat dilakukan, tetapi penaksir yang diperoleh bukanlah penaksir terbaik.
                             Penaksir yang memenuhi asumsi klasik akan bersifat BLUE (best linear unbiased estimator) yaitu penaksir linier, takbias terbaik karena memiliki varians minimum. "),
                     
                   )
                 )
          )
        ),
        box(width=12,
            title=strong("Analisis Inferensia"),
            fluidRow(
              column(
                width = 4,
                box(
                  width = NULL, height = "125px", status = "primary", solidHeader = TRUE,
                  title = "Pilih Provinsi",
                  selectInput(
                    inputId  = "prov_var",
                    label    = "Provinsi:",
                    choices = c(unique(data_dashboard$provinsi)),
                    selected = "Aceh",
                  )
                ),
                box(
                  width = NULL, height = "210px", status = "primary", solidHeader = TRUE,
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
                      plugins = list("remove_button"),
                      maxItems = NULL,
                      dropdownMaxHeight = "300px"
                    )
                  )
                ),
                
                # Box untuk Transformasi Variabel
                box(
                  width = NULL, status = "warning", solidHeader = TRUE,
                  title = "Transformasi Variabel",
                  
                  # Transformasi Y (Variabel Dependen)
                  h5("Transformasi (Y) : ", style = "color: #d9534f; font-weight: bold;"),
                  br(),
                  radioButtons(
                    inputId = "trans_dep",
                    label = NULL,
                    choices = list(
                      "Tanpa Transformasi" = "none",
                      "Logaritma Natural (ln)" = "log",
                      "Logaritma 10 (log10)" = "log10",
                      "Akar Kuadrat (sqrt)" = "sqrt",
                      "Kuadrat (x²)" = "square",
                      "Kubik (x³)" = "cubic",
                      "Reciprocal (1/x)" = "reciprocal",
                      "Box-Cox" = "boxcox",
                      "Yeo-Johnson" = "yeojohnson",
                      "Inverse Hyperbolic Sine" = "asinh",
                      "Logit" = "logit",
                      "Probit" = "probit"
                    ),
                    selected = "none",
                    inline = FALSE
                  ),
                  
                  br(),
                  
                  # Transformasi X (Variabel Independen)
                  h5("Transformasi (X) : ", style = "color: #5bc0de; font-weight: bold;"),
                  br(),
                  radioButtons(
                    inputId = "trans_indep",
                    label = NULL,
                    choices = list(
                      "Tanpa Transformasi" = "none",
                      "Logaritma Natural (ln)" = "log",
                      "Logaritma 10 (log10)" = "log10",
                      "Akar Kuadrat (sqrt)" = "sqrt",
                      "Kuadrat (x²)" = "square",
                      "Kubik (x³)" = "cubic",
                      "Reciprocal (1/x)" = "reciprocal",
                      "Box-Cox" = "boxcox",
                      "Yeo-Johnson" = "yeojohnson",
                      "Inverse Hyperbolic Sine" = "asinh",
                      "Standardisasi (z-score)" = "scale",
                      "Min-Max Normalisasi" = "minmax"
                    ),
                    selected = "none",
                    inline = FALSE
                  ),
                  
                  br(),
                  
                  # Tombol untuk reset transformasi
                  actionButton(
                    inputId = "reset_transform",
                    label = "Reset Transformasi",
                    icon = icon("refresh"),
                    class = "btn-warning",
                    style = "width: 100%;"
                  ),
                  
                  br(), br(),
                  
                  # Informasi tambahan
                  div(
                    style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; font-size: 12px;",
                    HTML("<strong>Tips:</strong><br/>
               • Transformasi variabel dapat dilakukan untuk menemukan model yang cocok serta memenuhi asumsi<br/>
               • Gunakan <strong>Box-Cox/Yeo-Johnson</strong> untuk normalitas<br/>
               • Gunakan <strong>reciprocal</strong> untuk hubungan non-linear<br/>
               • Gunakan <strong>standardisasi</strong> jika skala X berbeda jauh")
                  )
                )
              ),
              
              column(
                width = 8,
                box(
                  width = NULL, status = "info", solidHeader = TRUE,
                  title = "Ringkasan Model Regresi",
                  verbatimTextOutput("model_summary")
                ),
                
                box(
                  width = 12, status = "info", solidHeader = TRUE,
                  title = "Interpretasi Regresi",
                  verbatimTextOutput("interpretasi_regresi")
                ),
                
                box(
                  width = NULL, status = "success", solidHeader = TRUE,
                  title = "Statistik Deskriptif",
                  tagList(
                    verbatimTextOutput("descriptive_stats"),
                    verbatimTextOutput("inter_descriptive_stats")
                  )
                )
              )
            ),
            
            fluidRow(
              box(
                width = 12, status = "warning", solidHeader = TRUE,
                title = "Diagnostik Plot",
                plotOutput("diagnostic_plot")
              )
            )
        ),
        box(width=12,
            title=strong("Pemeriksaan Asumsi Klasik Model dan Multikolinearitas RLB"),
            fluidRow(
              box(
                width = 12, status = "danger", solidHeader = TRUE,
                title = "Ringkasan Pemeriksaan Uji Model Regresi",
                verbatimTextOutput("overall_check")
              )
            ),
            
            fluidRow(
              # Row 1: Homoskedastisitas dan Normalitas
              column(6,
                     box(
                       width = NULL, status = "danger", solidHeader = TRUE,
                       title = "Uji Homoskedastisitas",
                       verbatimTextOutput("assumption1")
                     )
              ),
              column(6,
                     box(
                       width = NULL, status = "danger", solidHeader = TRUE,
                       title = "Uji Normalitas Residual",
                       verbatimTextOutput("assumption2")
                     )
              )
            ),
            fluidRow(
              # Row 2: Multikolinearitas dan Autokorelasi
              column(6,
                     box(
                       width = NULL, status = "danger", solidHeader = TRUE,
                       title = "Uji Multikolinearitas",
                       verbatimTextOutput("assumption3")
                     )
              ),
              column(6,
                     box(
                       width = NULL, status = "danger", solidHeader = TRUE,
                       title = "Uji Autokorelasi",
                       verbatimTextOutput("assumption4")
                     )
              )
            ),
            # FITUR UNDUH LAPORAN ANALISIS REGRESI
            fluidRow(
              box(
                title = "Unduh Laporan",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                div(
                  style = "text-align: center; padding: 20px;",
                  p("Unduh laporan lengkap analisis regresi dalam format PDF"),
                  downloadButton("download_laporan_regresi", 
                                 "Unduh Laporan Analisis Regresi", 
                                 class = "btn-primary btn-lg",
                                 style = "margin: 10px;")
                )
              )
            )
        )
      ),
      
      tabItem(
        tabName = "stat-wilayah",
        fluidRow(
          column(width = 12,
                 box(
                   title = tags$strong("Petunjuk Penggunaan Halaman"),
                   status = "info",
                   width = 12,
                   icon = icon("book-open"),
                   p("Halaman ini menampilkan tabel dataset provinsi yang Anda pilih. Ikuti langkah-langkah berikut:"),
                   tags$ol(
                     tags$li(tags$strong("Pilih Provinsi:"), " Gunakan dropdown 'Pilih Provinsi' untuk melihat data spesifik suatu provinsi di Indonesia, atau pilih 'Indonesia' untuk melihat data agregat nasional."),
                     tags$li(tags$strong("Atur Rentang Tahun:"), " Geser slider 'Pilih Rentang Tahun' untuk memfokuskan data pada periode waktu yang Anda inginkan."),
                     tags$li(tags$strong("Unduh Data:"), " Setelah Anda memfilter provinsi dan rentang tahun, Anda dapat mengunduh tabel dataset sesuai filter Anda tadi."),
                     tags$li(tags$strong("Search:"), " Anda juga dapat memfilter secara manual tampilan data tabel dengan mengetikkan kata kunci, misal 'Aceh', '2022', dsb. di pojok kanan atas tabel.")
                   )
                 )
          )
        ),
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
        fluidRow(
          column(width = 12,
                 box(
                   title = tags$strong("Petunjuk Penggunaan Halaman"),
                   status = "info",
                   width = 12,
                   icon = icon("book-open"),
                   p("Halaman ini menampilkan tabel dataset jenis bencana yang Anda pilih. Ikuti langkah-langkah berikut:"),
                   tags$ol(
                     tags$li(tags$strong("Pilih Bencana:"), " Gunakan dropdown 'Pilih Bencana' untuk melihat data spesifik suatu bencana yang terjadi tiap provinsi di Indonesia."),
                     tags$li(tags$strong("Atur Rentang Tahun:"), " Geser slider 'Pilih Rentang Tahun' untuk memfokuskan data pada periode waktu yang Anda inginkan."),
                     tags$li(tags$strong("Unduh Data:"), " Setelah Anda memfilter jenis bencana dan rentang tahun, Anda dapat mengunduh tabel dataset sesuai filter Anda tadi."),
                     tags$li(tags$strong("Search:"), " Anda juga dapat memfilter secara manual tampilan data tabel dengan mengetikkan kata kunci, misal 'Aceh', '2022', dsb. di pojok kanan atas tabel.")
                   )
                 )
          )
        ),
        
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
              width = 4,
              sliderInput(
                inputId = "rentang_tahun_bencana_stat",
                label = "Pilih Rentang Tahun :",
                min = min(data_dashboard$tahun),
                max = max(data_dashboard$tahun),
                value = c(min(data_dashboard$tahun), max(data_dashboard$tahun)),
                sep = ""
              )
            ),
            
            column(
              width = 4,
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
      
      # --- HALAMAN METADATA ---
      tabItem(
        tabName = "metadata",
        fluidRow(
          
          column(width = 6,
                 box(
                   title = tagList(icon("file-alt"), "Sumber Data"),
                   status = "primary",
                   solidHeader = TRUE,
                   width = 12,
                   p("Data yang digunakan dalam dashboard ini merupakan gabungan dari beberapa sumber terpercaya untuk memastikan analisis yang komprehensif:"),
                   tags$ul(
                     tags$li(tags$strong("Badan Nasional Penanggulangan Bencana (BNPB):"), " Data jumlah kejadian dan korban jiwa untuk berbagai jenis bencana hidrometeorologi."),
                     tags$li(tags$strong("Kementrian Lingkungan Hidup dan Kehutanan:"), " Data jumlah kebakaran hutan dan lahan."),
                     tags$li(tags$strong("Badan Meteorologi, Klimatologi, dan Geofisika (BMKG):"), " Data historis variabel iklim seperti suhu, curah hujan, kelembaban, dll."),
                     tags$li(tags$strong("Badan Pusat Statistik (BPS):"), " Rujukan metadata variabel statistik yang digunakan ."),
                     tags$li(tags$strong("Data Spasial (Shapefile):"), " Peta administratif provinsi Indonesia untuk visualisasi geografis.")
                   )
                 )
          ),
          # Box Cakupan Data
          column(width = 6,
                 box(
                   title = tagList(icon("calendar-alt"), "Cakupan Data"),
                   status = "info",
                   solidHeader = TRUE,
                   width = 12,
                   p("Cakupan data yang dianalisis dalam dashboard ini meliputi:"),
                   tags$ul(
                     tags$li(tags$strong("Rentang Waktu:"), " 2020 – 2024."),
                     tags$li(tags$strong("Cakupan Wilayah:"), " 34, 37, hingga 38 Provinsi di seluruh Indonesia (disesuaikan dengan pemekaran wilayah)."),
                     tags$li(tags$strong("Frekuensi Update:"), " Data diperbarui setiap tahun."),
                     tags$li(tags$strong("Variabel Utama:"), " Data kejadian bencana, korban jiwa, dan berbagai parameter iklim.")
                   ),
                   p("Data telah melalui proses pembersihan dan agregasi ke tingkat tahunan per provinsi.")
                 )
          )
        ),
        
        # Box untuk Melihat Detail Variabel
        fluidRow(
          box(
            title = tagList(icon("tags"), "Eksplorasi Metadata Variabel"),
            status = "success",
            solidHeader = TRUE,
            width = 12,
            # Dropdown untuk memilih variabel
            selectInput(
              inputId = "pilih_variabel_metadata",
              label = "Pilih variabel untuk melihat detailnya:",
              # Mengambil nama variabel dari kamus data kita
              choices = setNames(names(metadata_kamus), sapply(metadata_kamus, `[[`, "Nama Variabel")),
              width = "100%"
            ),
            hr(),
            # Output untuk menampilkan detail metadata
            uiOutput("tampil_metadata_variabel"),
            p(strong("Sumber :"),"Badan Pusat Statistik (BPS)")
          )
        ),
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
  
  # Piechart
  output$pie_korban_plot <- renderPlotly({
    
    plot_data <- provinsi_filter() %>%
      select(
        korban_Banjir,
        korban_Cuaca_ekstrem,
        korban_Gelombang_pasang_Abrasi,
        korban_Kebakaran_hutan_dan_lahan,
        korban_Kekeringan,
        korban_Longsor
      ) %>%
      summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Jenis_Bencana",
        values_to = "Jumlah_Korban"
      ) %>%
      mutate(
        Jenis_Bencana = str_replace(Jenis_Bencana, "^korban_", ""),
        Jenis_Bencana = str_replace_all(Jenis_Bencana, "_", " "),
        Jenis_Bencana = str_to_title(Jenis_Bencana)
      ) %>%
      arrange(desc(Jumlah_Korban))
    
    if (nrow(plot_data) == 0 || sum(plot_data$Jumlah_Korban) == 0) {
      return(
        plot_ly() %>%
          layout(
            title = "Tidak ada data korban jiwa untuk ditampilkan",
            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
          )
      )
    }
    
    plot_ly(
      data = plot_data,
      labels = ~Jenis_Bencana,
      values = ~Jumlah_Korban,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      hovertemplate = '%{label}<br>Jumlah Korban: %{value:,}<extra></extra>'
    ) %>%
      layout(
        title = "Proporsi Korban Jiwa Berdasarkan Jenis Bencana"
      )
  })
  
  #PIE JUMLAH BENCANA
  output$pie_bencana_plot <- renderPlotly({
    
    plot_data <- provinsi_filter() %>%
      select(Banjir, `Cuaca ekstrem`, `Gelombang pasang / Abrasi`, `Kebakaran hutan dan lahan`, Kekeringan, Longsor) %>%
      summarise_all(sum, na.rm = TRUE) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Jenis_Bencana",
        values_to = "Total_Kejadian"
      ) %>%
      arrange(desc(Total_Kejadian))
    
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
      labels = ~Jenis_Bencana,
      values = ~Total_Kejadian,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      hovertemplate = '%{label}<br>Jumlah Kejadian: %{value:,}<extra></extra>'
    ) %>%
      layout(
        title = "Proporsi Jenis Bencana Berdasarkan Jumlah Kejadian"
      )
  })
  
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
      summarise(total_korban = sum(Total_Korban, na.rm = TRUE),.groups = 'drop')
    
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
      ) %>% 
      config(displayModeBar = FALSE)
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
      ) %>% 
      config(displayModeBar = FALSE)
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
      ) %>% 
      config(displayModeBar = FALSE)
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
      ) %>% 
      config(displayModeBar = FALSE)
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
      ) %>% 
      config(displayModeBar = FALSE)
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
      ) %>% 
      config(displayModeBar = FALSE)
  })
  
  ## Top 10 Provinsi Terdampak
  
  output$top_provinsi_box <- renderUI({
    box(
      title = paste("Top 10 Provinsi Terdampak", input$bencana_terpilih),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      plotlyOutput("top_provinsi_plot", height = "500px") 
    )
  })
  
  output$top_provinsi_plot <- renderPlotly({
    
    plot_data <- bencana_filter() %>%
      group_by(provinsi) %>%
      summarise(total_kejadian = sum(bencana_spesifik, na.rm = TRUE)) %>%
      filter(total_kejadian > 0) %>%
      arrange(desc(total_kejadian)) %>%
      slice_head(n = 10)
    
    if (nrow(plot_data) == 0) {
      return(
        plot_ly() %>%
          layout(title = "Tidak ada data untuk ditampilkan pada pilihan ini")
      )
    }
    
    plot_ly(
      data = plot_data,
      y = ~reorder(provinsi, total_kejadian),
      x = ~total_kejadian,
      type = 'bar',
      orientation = 'h',
      marker = list(color = '#007BFF'),
      hovertemplate = '%{y}<br>Jumlah Kejadian: %{x:,}<extra></extra>'
    ) %>%
      layout(
        xaxis = list(title = paste("Total Kejadian", input$bencana_terpilih)),
        yaxis = list(title = "")
      )
  })
  
  # Output Peta
  
  ## Filter Data Reaktif untuk Peta Bencana
  peta_bencana_filter <- reactive({
    bencana_spesifik <- input$peta_bencana_terpilih
    korban_spesifik <- paste0("korban_",gsub(" / ","_", bencana_spesifik))
    korban_spesifik <- gsub(" ","_", korban_spesifik)
    
    data_dashboard %>%
      filter(tahun == input$tahun_peta_bencana) %>% 
      select(
        provinsi,
        tahun,
        bencana_spesifik = all_of(bencana_spesifik),
        korban_spesifik = all_of(korban_spesifik)
      )
  })
  
  ## Peta dengan Leaflet
  output$peta_bencana_box<- renderUI({
    box(
      title = paste("Peta Distribusi", input$peta_bencana_terpilih,"Tahun",input$tahun_peta_bencana),
      status = "success",
      solidHeader = TRUE,
      width = 12,
      leafletOutput("peta_distribusi_bencana", height = "600px")
    )
  })
  
  output$peta_distribusi_bencana <- renderLeaflet({
    data_leaflet <- peta_bencana_filter() %>%
      group_by(provinsi) %>%
      summarise(total_bencana = sum(bencana_spesifik, na.rm = TRUE)) %>%
      filter(total_bencana > 0)
    
    if (input$tahun_peta_bencana < 2022) {
      peta_indonesia <- peta_indonesia_34provinsi
    } else if (input$tahun_peta_bencana >= 2022 && input$tahun_peta_bencana < 2024 ) {
      peta_indonesia <- peta_indonesia_37provinsi
    } else {
      peta_indonesia <- peta_indonesia_38provinsi
    }
    
    peta_indonesia <- left_join(peta_indonesia, data_leaflet, by = c("PROVINSI" = "provinsi"))
    
    pal <- colorNumeric(
      palette = "YlOrRd",
      domain = peta_indonesia$total_bencana
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s",
      peta_indonesia$PROVINSI,
      ifelse(
        is.na(peta_indonesia$total_bencana),
        "Tidak ada Kejadian",
        paste0(peta_indonesia$total_bencana, " kejadian")
      )
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
          weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, values = ~total_bencana, opacity = 0.7,
        title = paste("Jumlah Kejadian", input$peta_bencana_terpilih),
        position = "bottomright",
        na.label = "Tidak ada"
      )
  })
  
  ## Filter Data Reaktif untuk Peta Iklim
  peta_iklim_filter <-  reactive({
    nilai_iklim <- input$peta_iklim_terpilih
    
    data_dashboard %>% 
      filter(tahun == input$tahun_peta_iklim) %>% 
      select(
        provinsi,
        tahun,
        nilai_iklim = all_of(nilai_iklim)
      )
  })
  
  ## Peta dengan Leaflet
  output$peta_iklim_box <- renderUI({
    box(
      title = paste("Peta Distribusi", names(variabel_iklim)[variabel_iklim == input$peta_iklim_terpilih],"Tahun",input$tahun_peta_iklim),
      status = "success",
      solidHeader = TRUE,
      width = 12,
      leafletOutput("peta_distribusi_iklim", height = "600px")
    )
  })
  
  output$peta_distribusi_iklim <- renderLeaflet({
    data_leaflet <- peta_iklim_filter()
    
    if (input$tahun_peta_iklim < 2022) {
      peta_indonesia <- peta_indonesia_34provinsi
    } else if (input$tahun_peta_iklim >= 2022 && input$tahun_peta_iklim < 2024 ) {
      peta_indonesia <- peta_indonesia_37provinsi
    } else {
      peta_indonesia <- peta_indonesia_38provinsi
    }
    
    peta_indonesia <- left_join(peta_indonesia, data_leaflet, by = c("PROVINSI" = "provinsi"))
    
    pal <- colorNumeric(
      palette = "YlGnBu",
      domain = peta_indonesia$nilai_iklim
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %0.2f",
      peta_indonesia$PROVINSI,
      names(variabel_iklim)[variabel_iklim == input$peta_iklim_terpilih],
      peta_indonesia$nilai_iklim
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data = peta_indonesia) %>%
      addTiles() %>%
      setView(lng = 118, lat = -2, zoom = 4.5) %>%
      addPolygons(
        fillColor = ~pal(nilai_iklim),
        weight = 1, opacity = 1, color = "white",
        dashArray = "3", fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3, color = "#666",
          dashArray = "", fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px", direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, 
        values = ~nilai_iklim, 
        opacity = 0.7,
        title = names(variabel_iklim)[variabel_iklim == input$peta_iklim_terpilih],
        position = "bottomright",
        na.label = "Tidak Ada Data"
      )
  })
  
  # Output Analisis
  
  # Fungsi interpretasi korelasi
  interpret_korelasi <- function(data, vars, provinsi) {
    cat("Interpretasi Korelasi Antar Variabel untuk Provinsi:", provinsi, "\n\n")
    cor_matrix <- cor(data[, vars], use = "complete.obs")
    
    for (i in 1:(length(vars) - 1)) {
      for (j in (i + 1):length(vars)) {
        nilai <- cor_matrix[i, j]
        strength <- ifelse(abs(nilai) < 0.3, "lemah",
                           ifelse(abs(nilai) < 0.7, "sedang", "kuat"))
        direction <- ifelse(nilai > 0, "positif", "negatif")
        cat("Variabel", vars[i], "dan", vars[j], "memiliki korelasi", strength, direction,
            "(", round(nilai, 2), ")\n")
      }
    }
  }
  
  ## Scatter-Matrix
  output$scatterplot <- renderPlot({
    req(input$vars_korelasi)
    validate(need(length(input$vars_korelasi) >= 2,
                  "Pilih minimal 2 variabel."))
    
    # --- filter provinsi (sama dgn regresi) ---
    if (input$prov_korelasi == "Indonesia") {
      dat <- data_dashboard
    } else {
      dat <- data_dashboard %>% filter(provinsi == input$prov_korelasi)
    }
    
    # --- terapkan transformasi jika user mau (opsional) ---
    trans_x <- input$trans_indep  # dari panel regresi
    trans_y <- input$trans_dep
    for (v in input$vars_korelasi) {
      if (v == input$dep_var) {  
        # variabel yang sama dengan Y terpilih
        dat[[v]] <- apply_transformation(dat[[v]], trans_y)
      } else {                           # selain itu pakai transformasi X
        dat[[v]] <- apply_transformation(dat[[v]], trans_x)
      }
    }
    
    # siap diplot
    ggpairs(dat[, input$vars_korelasi, drop = FALSE])
  })
  
  output$cor_matrix <- renderDT({
    req(input$vars_korelasi)
    validate(need(length(input$vars_korelasi) >= 2, ""))
    
    # filter provinsi—sama persis dg di atas
    dat <- if (input$prov_korelasi == "Indonesia") data_dashboard
    else filter(data_dashboard, provinsi == input$prov_korelasi)
    
    # transformasi identik
    for (v in input$vars_korelasi) {
      if (v == input$dep_var)  dat[[v]] <- apply_transformation(dat[[v]], input$trans_dep)
      else                     dat[[v]] <- apply_transformation(dat[[v]], input$trans_indep)
    }
    
    cor_mat <- round(cor(dat[, input$vars_korelasi], use = "pairwise.complete.obs"), 3)
    colnames(cor_mat) <- rownames(cor_mat) <- format_label(colnames(cor_mat))
    datatable(cor_mat, options = list(dom = "t", scrollX = TRUE))
  })
  
  # Fungsi interpretasi regresi
  interpret_regresi <- function(model, dep_var, indep_vars, provinsi) {
    cat("Interpretasi Regresi untuk Provinsi:", provinsi, "\n\n")
    coef <- summary(model)$coefficients
    
    for (i in 2:nrow(coef)) {
      var_name <- indep_vars[i - 1]
      beta <- coef[i, 1]
      pval <- coef[i, 4]
      
      signif <- if (pval < 0.01) "sangat signifikan"
      else if (pval < 0.05) "signifikan"
      else "tidak signifikan"
      
      arah <- if (beta > 0) "meningkatkan" else "menurunkan"
      
      cat(sprintf("Setiap kenaikan 1 unit %s akan %s %s sebesar %.3f (p = %.3f), yang berarti %s.\n",
                  var_name, arah, dep_var, abs(beta), pval, signif))
    }
  }
  
  # Regresi dengan Filter Provinsi
  
  # Fungsi helper untuk transformasi
  apply_transformation <- function(x, trans_type, lambda = NULL) {
    switch(trans_type,
           "none" = x,
           "log" = {
             # Pastikan tidak ada nilai <= 0
             if (any(x <= 0, na.rm = TRUE)) {
               log(x + abs(min(x, na.rm = TRUE)) + 1)
             } else {
               log(x)
             }
           },
           "log10" = {
             if (any(x <= 0, na.rm = TRUE)) {
               log10(x + abs(min(x, na.rm = TRUE)) + 1)
             } else {
               log10(x)
             }
           },
           "sqrt" = {
             if (any(x < 0, na.rm = TRUE)) {
               sqrt(x + abs(min(x, na.rm = TRUE)))
             } else {
               sqrt(x)
             }
           },
           "square" = x^2,
           "cubic" = x^3,
           "reciprocal" = {
             ifelse(x == 0, NA, 1/x)
           },
           "boxcox" = {
             if (is.null(lambda)) {
               # Otomatis hitung lambda optimal
               if (any(x <= 0, na.rm = TRUE)) {
                 x_shifted <- x + abs(min(x, na.rm = TRUE)) + 1
               } else {
                 x_shifted <- x
               }
               tryCatch({
                 lambda <- BoxCox.lambda(x_shifted, method = "guerrero")
                 if (lambda == 0) log(x_shifted) else (x_shifted^lambda - 1) / lambda
               }, error = function(e) x)
             } else {
               if (lambda == 0) log(x) else (x^lambda - 1) / lambda
             }
           },
           "yeojohnson" = {
             # Implementasi Yeo-Johnson transformation
             tryCatch({
               lambda <- 0  # bisa disesuaikan
               ifelse(x >= 0,
                      ifelse(lambda == 0, log(x + 1), ((x + 1)^lambda - 1) / lambda),
                      ifelse(lambda == 2, -log(-x + 1), -((-x + 1)^(2-lambda) - 1) / (2 - lambda)))
             }, error = function(e) x)
           },
           "asinh" = asinh(x),
           "logit" = {
             # Untuk proporsi (0,1), jika diluar range akan direscale
             x_rescaled <- rescale(x, to = c(0.001, 0.999))
             log(x_rescaled / (1 - x_rescaled))
           },
           "probit" = {
             x_rescaled <- rescale(x, to = c(0.001, 0.999))
             qnorm(x_rescaled)
           },
           "scale" = as.numeric(scale(x)),
           "minmax" = rescale(x, to = c(0, 1)),
           x  # default return original
    )
  }
  
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
  
  # Observer untuk reset transformasi
  observeEvent(input$reset_transform, {
    updateRadioButtons(session, "trans_dep", selected = "none")
    updateRadioButtons(session, "trans_indep", selected = "none")
  })
  
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
    default_x <- default_x[default_x %in% pilihan]
    
    # Jika tidak ada default, ambil 3 pertama dari sisa variabel
    if (is.null(default_x) || length(default_x) == 0) {
      default_x <- pilihan[1:min(3, length(pilihan))]
    }
    
    updateSelectInput(session, "indep_vars",
                      choices  = setNames(pilihan, pilihan_formatted),
                      selected = default_x
    )
  }, ignoreInit = TRUE)
  
  # Model fit dengan transformasi
  model_fit <- reactive({
    req(input$dep_var, input$indep_vars, input$prov_var)
    validate(need(length(input$indep_vars) >= 1, "Pilih minimal 1 variabel independen."))
    
    # # Filter data
    # if (input$prov_var == "Indonesia") {
    #   data_filtered <- data_dashboard
    # } else {
    #   data_dashboard[is.na(data_dashboard)] <-  0 
    #   data_filtered <- data_dashboard[data_dashboard$provinsi == input$prov_var, ]
    # }
    
    data_filtered <- data_dashboard %>%
      # BENAR: Lakukan mutasi di sini, bukan pada objek global
      mutate(across(everything(), ~replace_na(.x, 0))) %>%
      filter(provinsi == input$prov_var)
    
    validate(need(nrow(data_filtered) > 0,
                  paste("Tidak ada data untuk provinsi:", input$prov_var)))
    
    validate(need(nrow(data_filtered) > length(input$indep_vars),
                  "Jumlah observasi tidak mencukupi untuk analisis regresi"))
    
    # Siapkan data untuk transformasi
    data_model <- data_filtered[, c(input$dep_var, input$indep_vars), drop = FALSE]
    
    # Transformasi Y
    y_transformed <- apply_transformation(data_model[[input$dep_var]], input$trans_dep)
    
    # Transformasi X
    x_transformed <- data_model[, input$indep_vars, drop = FALSE]
    for (i in 1:ncol(x_transformed)) {
      x_transformed[[i]] <- apply_transformation(x_transformed[[i]], input$trans_indep)
    }
    
    # Gabungkan data
    data_final <- data.frame(
      y = y_transformed,
      x_transformed
    )
    
    # Buat formula
    formula_str <- paste("y ~", paste(colnames(x_transformed), collapse = " + "))
    
    # Jalankan model
    tryCatch({
      lm(as.formula(formula_str), data = data_final)
    }, error = function(e) {
      list(error = paste("Error dalam model:", e$message))
    })
  })
  
  # OUTPUT: Model summary dengan informasi transformasi
  output$model_summary <- renderPrint({
    model <- model_fit()
    
    if (is.list(model) && !is.null(model$error)) {
      cat("ERROR:", model$error, "\n")
      cat("Coba gunakan transformasi yang berbeda atau periksa data Anda.\n")
      return()
    }
    
    # Informasi transformasi
    cat("==================== INFORMASI TRANSFORMASI ====================\n")
    cat("Variabel Y:", input$dep_var, 
        ifelse(input$trans_dep == "none", "", paste("(Transformasi:", input$trans_dep, ")")), "\n")
    cat("Variabel X:", paste(input$indep_vars, collapse = ", "), 
        ifelse(input$trans_indep == "none", "", paste("(Transformasi:", input$trans_indep, ")")), "\n")
    cat("======================= RINGKASAN MODEL ========================\n")
    
    print(summary(model))
  })
  
  # Interpretasi regresi
  output$interpretasi_regresi <- renderPrint({
    model <- model_fit()
    if (inherits(model, "lm")) {
      interpret_regresi(model, input$dep_var, input$indep_vars, input$prov_var)
    } else {
      cat("Model tidak valid.")
    }
  })
  
  # Interpretasi korelasi
  output$interpretasi_korelasi <- renderPrint({
    req(input$vars_korelasi, input$prov_korelasi)
    validate(need(length(input$vars_korelasi) >= 2, "Pilih minimal 2 variabel."))
    
    dat <- filter(data_dashboard, provinsi == input$prov_korelasi)
    
    # Apply transformasi jika perlu
    for (v in input$vars_korelasi) {
      if (v == input$dep_var)  dat[[v]] <- apply_transformation(dat[[v]], input$trans_dep)
      else                     dat[[v]] <- apply_transformation(dat[[v]], input$trans_indep)
    }
    
    interpret_korelasi(dat, input$vars_korelasi, input$prov_korelasi)
  })
  
  
  # reactive untuk menampilkan statistik deskriptif data yang difilter
  filtered_data <- reactive({
    req(input$prov_var)
    data_dashboard[is.na(data_dashboard)] <-  0 
    data_dashboard[data_dashboard$provinsi == input$prov_var,]
  })
  
  # Output untuk statistik deskriptif
  output$descriptive_stats <- renderPrint({
    req(input$dep_var, input$indep_vars)
    
    data_filtered <- filtered_data()
    selected_vars <- c(input$dep_var, input$indep_vars)
    
    cat("Provinsi:", input$prov_var, "\n")
    cat("Jumlah Observasi:", nrow(data_filtered), "\n\n")
    
    print(summary(data_filtered[, selected_vars, drop = FALSE]))
  })
  
  # Output untuk interpretasi statistik deskriptif
  output$inter_descriptive_stats <- renderPrint({
    req(input$dep_var, input$indep_vars)
    
    data_filtered <- filtered_data()
    selected_vars <- c(input$dep_var, input$indep_vars)
    
    # Interpretasi untuk setiap variabel
    for (var in selected_vars) {
      var_data <- data_filtered[[var]]
      var_data <- var_data[!is.na(var_data)]  # Hapus NA
      
      if (length(var_data) == 0) {
        cat(paste("•", var, ": Tidak ada data yang valid\n\n"))
        next
      }
      
      # Hitung statistik
      mean_val <- mean(var_data, na.rm = TRUE)
      median_val <- median(var_data, na.rm = TRUE)
      sd_val <- sd(var_data, na.rm = TRUE)
      min_val <- min(var_data, na.rm = TRUE)
      max_val <- max(var_data, na.rm = TRUE)
      q1 <- quantile(var_data, 0.25, na.rm = TRUE)
      q3 <- quantile(var_data, 0.75, na.rm = TRUE)
      iqr_val <- q3 - q1
      cv <- (sd_val / mean_val) * 100  # Coefficient of variation
      
      cat(paste("•", var, ":\n"))
      
      # Interpretasi tendensi sentral
      if (abs(mean_val - median_val) / sd_val < 0.1) {
        cat("  - Distribusi: Relatif simetris (rata-rata ≈ median)\n")
      } else if (mean_val > median_val) {
        cat("  - Distribusi: Condong ke kanan (skewed right) - ada nilai ekstrem tinggi\n")
      } else {
        cat("  - Distribusi: Condong ke kiri (skewed left) - ada nilai ekstrem rendah\n")
      }
      
      # Interpretasi variabilitas
      if (cv < 15) {
        cat("  - Variabilitas: Rendah (CV < 15%) - data relatif homogen\n")
      } else if (cv < 35) {
        cat("  - Variabilitas: Sedang (CV 15-35%) - data cukup beragam\n")
      } else {
        cat("  - Variabilitas: Tinggi (CV > 35%) - data sangat beragam\n")
      }
      
      # Interpretasi rentang
      cat(paste("  - Rentang:", round(max_val - min_val, 2),
                "dengan IQR:", round(iqr_val, 2), "\n"))
      
      # Deteksi outlier potensial
      lower_fence <- q1 - 1.5 * iqr_val
      upper_fence <- q3 + 1.5 * iqr_val
      outliers <- sum(var_data < lower_fence | var_data > upper_fence)
      
      if (outliers > 0) {
        cat(paste("  - Outlier: Terdeteksi", outliers, "nilai ekstrem potensial\n"))
      } else {
        cat("  - Outlier: Tidak terdeteksi nilai ekstrem\n")
      }
      
      # Interpretasi kontekstual berdasarkan jenis variabel
      if (var == input$dep_var) {
        cat("  - Status: Variabel DEPENDEN (yang diprediksi)\n")
        if (mean_val < 5) {
          cat("  - Kondisi: Frekuensi kejadian relatif rendah\n\n")
        } else if (mean_val < 20) {
          cat("  - Kondisi: Frekuensi kejadian sedang\n\n")
        } else {
          cat("  - Kondisi: Frekuensi kejadian tinggi\n\n")
        }
      } else {
        cat("  - Status: Variabel INDEPENDEN (prediktor)\n")
        
        # Interpretasi khusus untuk variabel cuaca
        if (grepl("suhu", var, ignore.case = TRUE)) {
          if (mean_val < 25) {
            cat("  - Kondisi: Suhu rata-rata relatif sejuk\n\n")
          } else if (mean_val < 30) {
            cat("  - Kondisi: Suhu rata-rata normal\n\n")
          } else {
            cat("  - Kondisi: Suhu rata-rata relatif panas\n\n")
          }
        } else if (grepl("hujan", var, ignore.case = TRUE)) {
          if (mean_val < 100) {
            cat("  - Kondisi: Curah hujan relatif rendah\n\n")
          } else if (mean_val < 300) {
            cat("  - Kondisi: Curah hujan sedang\n\n")
          } else {
            cat("  - Kondisi: Curah hujan tinggi\n\n")
          }
        } else if (grepl("kelembaban", var, ignore.case = TRUE)) {
          if (mean_val < 70) {
            cat("  - Kondisi: Kelembaban relatif rendah\n\n")
          } else if (mean_val < 85) {
            cat("  - Kondisi: Kelembaban normal\n\n")
          } else {
            cat("  - Kondisi: Kelembaban tinggi\n\n")
          }
        } else if (grepl("angin", var, ignore.case = TRUE)) {
          if (mean_val < 5) {
            cat("  - Kondisi: Kecepatan angin relatif tenang\n\n")
          } else if (mean_val < 15) {
            cat("  - Kondisi: Kecepatan angin sedang\n\n")
          } else {
            cat("  - Kondisi: Kecepatan angin kencang\n\n")
          }
        }
      }
    }
  })
  
  output$diagnostic_plot <- renderPlot({
    par(mfrow = c(2, 2))
    plot(model_fit())
    par(mfrow = c(1, 1))
  })
  
  # Reactive values untuk menyimpan hasil pengujian
  assumption_results <- reactiveValues(
    homoskedastisitas = NULL,
    normalitas = NULL,
    multikolinearitas = NULL,
    autokorelasi = NULL
  )
  
  output$assumption1 <- renderPrint({
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
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Breusch-Pagan lebih besar dari α, sehingga H₀ diterima.\n")
        cat("Dengan demikian, model regresi memenuhi asumsi homoskedastisitas atau variansi residual dianggap konstan.\n")
        assumption_results$homoskedastisitas <- TRUE
      } else {
        cat("   - Keputusan = TOLAK H₀ (p ≤ 0.05)\n\n")
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Breusch-Pagan kurang dari atau sama dengan α, sehingga H₀ ditolak.\n")
        cat("Dengan demikian, model regresi belum memenuhi asumsi homoskedastisitas atau terdapat heteroskedastisitas pada model.\n")
        assumption_results$homoskedastisitas <- FALSE
      }
    }, error = function(e) {
      cat("   Error dalam uji Breusch-Pagan:", e$message, "\n")
      assumption_results$homoskedastisitas <- FALSE
    })
  })
  
  output$assumption2 <- renderPrint({
    req(model_fit())
    
    model <- model_fit()
    
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
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Shapiro-Wilk lebih besar dari α, sehingga H₀ diterima.\n")
        cat("Dengan demikian, model regresi memenuhi asumsi normalitas error atau residual berdistribusi normal.\n")
        assumption_results$normalitas <- TRUE
      } else {
        cat("   - Keputusan = TOLAK H₀ (p ≤ 0.05)\n\n")
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Shapiro-Wilk kurang dari atau sama dengan α, sehingga H₀ ditolak.\n")
        cat("Dengan demikian, model regresi belum memenuhi asumsi normalitas error atau residual tidak berdistribusi normal.\n")
        assumption_results$normalitas <- FALSE
      }
    } else {
      # Kolmogorov-Smirnov Test untuk sampel besar
      ks_test <- ks.test(residuals, "pnorm", mean(residuals), sd(residuals))
      cat("   Kolmogorov-Smirnov Test:\n")
      cat("   - Statistics KS =", round(ks_test$statistic, 4), "\n")
      cat("   - p-value =", round(ks_test$p.value, 4), "\n")
      
      if(ks_test$p.value > 0.05) {
        cat("   - Keputusan = GAGAL TOLAK H₀ (p > 0.05)\n\n")
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Kolmogorov-Smirnov lebih besar dari α, sehingga H₀ diterima.\n")
        cat("Dengan demikian, model regresi memenuhi asumsi normalitas error atau residual berdistribusi normal.\n")
        assumption_results$normalitas <- TRUE
      } else {
        cat("   - Keputusan = TOLAK H₀ (p ≤ 0.05)\n\n")
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Kolmogorov-Smirnov kurang dari atau sama dengan α, sehingga H₀ ditolak.\n")
        cat("Dengan demikian, model regresi belum memenuhi asumsi normalitas error atau residual tidak berdistribusi normal.\n")
        assumption_results$normalitas <- FALSE
      }
    }
  })
  
  output$assumption3 <- renderPrint({
    req(model_fit())
    
    model <- model_fit()
    
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
          cat("Dengan demikian, model regresi memenuhi asumsi nonmultikolinearitas\n")
          assumption_results$multikolinearitas <- TRUE
        } else if(max_vif <= 10) {
          cat("\n   Keputusan = TERDAPAT MULTIKOLINEARITAS SEDANG PADA MODEL (ada VIF 5-10)\n\n")
          cat("Dengan demikian, model regresi belum memenuhi asumsi nonmultikolinearitas.\n")
          assumption_results$multikolinearitas <- FALSE
        } else {
          cat("\n   Keputusan = TERDAPAT MULTI KOLINEARITAS TINGGI PADA MODEL (ada VIF > 10)\n\n")
          cat("Dengan demikian, model regresi belum memenuhi asumsi nonmultikolinearitas.\n")
          assumption_results$multikolinearitas <- FALSE
        }
      } else {
        cat("   Hanya ada 1 variabel independen, tidak perlu uji multikolinearitas.\n")
        assumption_results$multikolinearitas <- TRUE
      }
    }, error = function(e) {
      cat("   Error dalam uji VIF:", e$message, "\n")
      assumption_results$multikolinearitas <- FALSE
    })
  })
  
  output$assumption4 <- renderPrint({
    req(model_fit())
    
    model <- model_fit()
    
    # 4. UJI AUTOKORELASI (Durbin-Watson Test)
    cat("4. UJI AUTOKORELASI\n")
    cat("   H₀: Tidak ada autokorelasi (ρ = 0)\n")
    cat("   H₁: Ada autokorelasi (ρ ≠ 0)\n")
    
    tryCatch({
      library(lmtest)
      dw_test <- dwtest(model)
      cat("   Durbin-Watson Test:\n")
      cat("   - Statistics DW =", round(dw_test$statistic, 4), "\n")
      cat("   - p-value =", round(dw_test$p.value, 4), "\n")
      
      if(dw_test$p.value > 0.05) {
        cat("   - Keputusan = GAGAL TOLAK H₀ (p > 0.05)\n\n")
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Durbin-Watson lebih besar dari α, sehingga H₀ diterima.\n")
        cat("Dengan demikian, model regresi memenuhi asumsi nonautokorelasi atau tidak ada autokorelasi pada model.\n")
        assumption_results$autokorelasi <- TRUE
      } else {
        cat("   - Keputusan = TOLAK H₀ (p ≤ 0.05)\n\n")
        cat("Berdasarkan tingkat signifikansi 5%, p-value uji Durbin-Watson kurang dari atau sama dengan α, sehingga H₀ ditolak.\n")
        cat("Dengan demikian, model regresi belum memenuhi asumsi nonautokorelasi atau terdapat autokorelasi pada model.\n")
        assumption_results$autokorelasi <- FALSE
      }
    }, error = function(e) {
      cat("   Error dalam uji Durbin-Watson:", e$message, "\n")
      assumption_results$autokorelasi <- FALSE
    })
  })
  
  output$overall_check <- renderPrint({
    req(model_fit())
    
    cat("RINGKASAN PEMERIKSAAN ASUMSI KLASIK MODEL REGRESI\n")
    cat("================================================================\n\n")
    
    # Tunggu sampai semua pengujian selesai
    req(assumption_results$homoskedastisitas, assumption_results$normalitas, 
        assumption_results$multikolinearitas, assumption_results$autokorelasi)
    
    # Hitung jumlah asumsi yang terpenuhi
    assumptions <- c(
      "Homoskedastisitas" = assumption_results$homoskedastisitas,
      "Normalitas Residual" = assumption_results$normalitas,
      "Non-multikolinearitas" = assumption_results$multikolinearitas,
      "Non-autokorelasi" = assumption_results$autokorelasi
    )
    
    passed <- sum(assumptions, na.rm = TRUE)
    total <- length(assumptions)
    
    cat("STATUS ASUMSI:\n")
    for(i in 1:length(assumptions)) {
      status <- if(assumptions[i]) "✓ TERPENUHI" else "✗ TIDAK TERPENUHI"
      cat(sprintf("  %d. %-20s: %s\n", i, names(assumptions)[i], status))
    }
    
    cat("\n================================================================\n")
    cat("KESIMPULAN UMUM:\n")
    cat(sprintf("Model memenuhi %d dari %d asumsi klasik (%.1f%%)\n\n", 
                passed, total, (passed/total)*100))
    
    if(passed == total) {
      cat("🎉 EXCELLENT! Model regresi memenuhi SEMUA asumsi klasik.\n")
      cat("   Penaksir yang dihasilkan bersifat BLUE (Best Linear Unbiased Estimator).\n")
      cat("   Model dapat digunakan untuk inferensi dan prediksi dengan tingkat kepercayaan tinggi.\n")
    } else if(passed >= total * 0.75) {
      cat("✅ GOOD! Model regresi memenuhi sebagian besar asumsi klasik.\n")
      cat("   Penaksir masih dapat diandalkan, namun perlu perhatian pada asumsi yang tidak terpenuhi.\n")
      cat("   Pertimbangkan transformasi data atau metode regresi alternatif untuk asumsi yang dilanggar.\n")
    } else if(passed >= total * 0.5) {
      cat("⚠️  CAUTION! Model regresi hanya memenuhi sebagian asumsi klasik.\n")
      cat("   Penaksir mungkin tidak optimal. Hasil inferensi harus diinterpretasi dengan hati-hati.\n")
      cat("   Sangat disarankan untuk melakukan transformasi data atau menggunakan metode regresi robust.\n")
    } else {
      cat("❌ WARNING! Model regresi melanggar sebagian besar asumsi klasik.\n")
      cat("   Penaksir tidak bersifat BLUE dan hasil inferensi tidak dapat diandalkan.\n")
      cat("   Wajib melakukan perbaikan model sebelum digunakan untuk analisis lebih lanjut.\n")
    }
    
    cat("\n================================================================\n")
    cat("REKOMENDASI:\n")
    
    if(!assumption_results$normalitas) {
      cat("• Normalitas: Coba transformasi Box-Cox, Yeo-Johnson, atau log pada variabel dependen\n")
    }
    if(!assumption_results$homoskedastisitas) {
      cat("• Homoskedastisitas: Gunakan Weighted Least Squares (WLS) atau transformasi variabel\n")
    }
    if(!assumption_results$multikolinearitas) {
      cat("• Multikolinearitas: Hapus variabel dengan VIF tinggi atau gunakan Ridge Regression\n")
    }
    if(!assumption_results$autokorelasi) {
      cat("• Autokorelasi: Tambahkan lag variabel atau gunakan Generalized Least Squares (GLS)\n")
    }
    
    if(passed == total) {
      cat("• Model sudah optimal! Lanjutkan dengan interpretasi dan prediksi.\n")
    }
  })
  
  # Output Tabel Dinamis
  
  ## Filter Data Reaktif untuk Tabel Provinsi
  tabel_provinsi_filter <- reactive({
    if (input$provinsi_terpilih_stat == "Indonesia") {
      data_dashboard %>%
        filter(tahun >= input$rentang_tahun_provinsi_stat[1] & tahun <= input$rentang_tahun_provinsi_stat[2])
    } else {
      data_dashboard %>%
        filter(provinsi == input$provinsi_terpilih_stat) %>%
        filter(tahun >= input$rentang_tahun_provinsi_stat[1] & tahun <= input$rentang_tahun_provinsi_stat[2])
    }
  })
  
  ## Tabel Dinamis Provinsi
  output$tabel_stat_provinsi <- DT::renderDataTable({
    DT::datatable(
      tabel_provinsi_filter(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      filter = 'top',
      rownames = FALSE
    )
  })
  
  ## Download Data Provinsi
  output$download_data_provinsi <- downloadHandler(
    filename = function() {
      paste("data_provinsi_", input$provinsi_terpilih_stat, "_", 
            input$rentang_tahun_provinsi_stat[1], "-", input$rentang_tahun_provinsi_stat[2], 
            "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tabel_provinsi_filter(), file, row.names = FALSE)
    }
  )
  
  ## Filter Data Reaktif untuk Tabel Bencana
  tabel_bencana_filter <- reactive({
    bencana_spesifik <- input$bencana_terpilih_stat
    korban_spesifik <- paste0("korban_", gsub(" / ", "_", bencana_spesifik))
    korban_spesifik <- gsub(" ", "_", korban_spesifik)
    
    data_dashboard %>%
      filter(tahun >= input$rentang_tahun_bencana_stat[1] & tahun <= input$rentang_tahun_bencana_stat[2]) %>%
      select(
        provinsi,
        tahun,
        bencana_spesifik = all_of(bencana_spesifik),
        korban_spesifik = all_of(korban_spesifik)
      ) %>%
      rename(
        !!paste("Kejadian", bencana_spesifik) := bencana_spesifik,
        !!paste("Korban", bencana_spesifik) := korban_spesifik
      )
  })
  
  ## Tabel Dinamis Bencana
  output$tabel_stat_bencana <- DT::renderDataTable({
    DT::datatable(
      tabel_bencana_filter(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
      ),
      filter = 'top',
      rownames = FALSE
    )
  })
  
  ## Download Data Bencana
  output$download_data_bencana <- downloadHandler(
    filename = function() {
      paste("data_bencana_", gsub(" ", "_", input$bencana_terpilih_stat), "_", 
            input$rentang_tahun_bencana_stat[1], "-", input$rentang_tahun_bencana_stat[2], 
            "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tabel_bencana_filter(), file, row.names = FALSE)
    }
  )
  
  # Output Metadata
  
  output$tampil_metadata_variabel <- renderUI({
    req(input$pilih_variabel_metadata)
    
    # Ambil metadata untuk variabel yang dipilih
    metadata <- metadata_kamus[[input$pilih_variabel_metadata]]
    
    # Buat tampilan yang rapi
    div(
      style = "background-color: #f8f9fa; padding: 20px; border-radius: 10px; border-left: 5px solid #28a745;",
      
      # Nama Variabel sebagai header
      h4(metadata$`Nama Variabel`, 
         style = "color: #28a745; margin-bottom: 20px; font-weight: bold;"),
      
      # Detail metadata dalam format yang rapi
      div(
        style = "display: grid; gap: 15px;",
        
        div(
          strong("Konsep: "), 
          span(metadata$Konsep, style = "color: #495057;")
        ),
        
        div(
          strong("Definisi: "), 
          span(metadata$Definisi, style = "color: #495057; text-align: justify;")
        ),
        
        div(
          strong("Referensi Waktu: "), 
          span(metadata$`Referensi Waktu`, style = "color: #495057;")
        ),
        
        div(
          strong("Ukuran: "), 
          span(metadata$Ukuran, style = "color: #495057;")
        ),
        
        div(
          strong("Satuan: "), 
          span(metadata$Satuan, style = "color: #495057;")
        ),
        
        div(
          strong("Tipe Data: "), 
          span(metadata$`Tipe Data`, style = "color: #495057;")
        )
      )
    )
  })
  
  # Profile Tim
  
  # Data profil tim
  team_profiles <- list(
    list(
      id = "profile1",
      name = "Aisyah Nur Azizah",
      nim = "222212345",
      role = "Project Manager & Data Analyst",
      email = "aisyah.azizah@stis.ac.id",
      phone = "+62 812-3456-7890",
      skills = "R Programming, Data Visualization, Project Management",
      hobby = "Reading, Traveling, Photography",
      motto = "Data tells a story, and I love to listen.",
      image = "https://via.placeholder.com/300x300/667eea/ffffff?text=AA"
    ),
    list(
      id = "profile2", 
      name = "Budi Santoso",
      nim = "222212346",
      role = "Backend Developer & Database Specialist",
      email = "budi.santoso@stis.ac.id",
      phone = "+62 813-4567-8901",
      skills = "R Shiny, Database Management, Statistical Analysis",
      hobby = "Gaming, Coding, Music",
      motto = "Code is poetry written in logic.",
      image = "https://via.placeholder.com/300x300/764ba2/ffffff?text=BS"
    ),
    list(
      id = "profile3",
      name = "Citra Dewi",
      nim = "222212347", 
      role = "UI/UX Designer & Frontend Developer",
      email = "citra.dewi@stis.ac.id",
      phone = "+62 814-5678-9012",
      skills = "UI/UX Design, HTML/CSS, Data Visualization",
      hobby = "Design, Art, Yoga",
      motto = "Good design is invisible, great design is unforgettable.",
      image = "https://via.placeholder.com/300x300/f093fb/ffffff?text=CD"
    ),
    list(
      id = "profile4",
      name = "Dimas Pratama", 
      nim = "222212348",
      role = "Data Scientist & Research Analyst",
      email = "dimas.pratama@stis.ac.id",
      phone = "+62 815-6789-0123",
      skills = "Machine Learning, Statistical Modeling, Research",
      hobby = "Research, Sports, Technology",
      motto = "In data we trust, in analysis we excel.",
      image = "https://via.placeholder.com/300x300/4facfe/ffffff?text=DP"
    ),
    list(
      id = "profile5",
      name = "Eka Sari",
      nim = "222212349",
      role = "Quality Assurance & Documentation Specialist", 
      email = "eka.sari@stis.ac.id",
      phone = "+62 816-7890-1234",
      skills = "Quality Assurance, Technical Writing, Testing",
      hobby = "Writing, Reading, Cooking",
      motto = "Quality is not an act, it is a habit.",
      image = "https://via.placeholder.com/300x300/00d2ff/ffffff?text=ES"
    ),
    list(
      id = "profile6",
      name = "Fajar Nugroho",
      nim = "222212350",
      role = "DevOps & System Administrator",
      email = "fajar.nugroho@stis.ac.id", 
      phone = "+62 817-8901-2345",
      skills = "System Administration, DevOps, Cloud Computing",
      hobby = "Technology, Automation, Hiking",
      motto = "Automate everything, optimize always.",
      image = "https://via.placeholder.com/300x300/ff9a9e/ffffff?text=FN"
    )
  )
  
  # Reactive values untuk state management
  profile_state <- reactiveValues(
    current_view = "overview",
    selected_profile = NULL
  )
  
  # Observer untuk tombol detail
  observe({
    for(profile in team_profiles) {
      local({
        current_profile <- profile
        observeEvent(input[[paste0("btn_detail_", current_profile$id)]], {
          profile_state$current_view <- "detail"
          profile_state$selected_profile <- current_profile
        })
      })
    }
  })
  
  # Observer untuk tombol back
  observeEvent(input$btn_back_overview, {
    profile_state$current_view <- "overview"
    profile_state$selected_profile <- NULL
  })
  
  # Main profile UI output
  output$profileUI <- renderUI({
    div(class = "profile-content",
        if(profile_state$current_view == "overview") {
          # Overview page
          div(
            h2("Tim Pengembang Dashboard Climate Change", 
               style = "text-align: center; color: #2c3e50; margin-bottom: 40px; font-weight: bold;"),
            
            fluidRow(
              lapply(team_profiles, function(profile) {
                column(4,
                       div(class = "profile-card",
                           div(style = "text-align: center;",
                               img(src = profile$image, 
                                   style = "width: 120px; height: 120px; margin-bottom: 15px;"),
                               h4(profile$name),
                               p(profile$role, style = "color: #667eea; font-weight: 600;"),
                               p(profile$nim, style = "color: #95a5a6; font-size: 13px;"),
                               actionButton(paste0("btn_detail_", profile$id), 
                                            "Lihat Detail", 
                                            class = "btn-profile")
                           )
                       )
                )
              })
            )
          )
        } else {
          # Detail page
          profile <- profile_state$selected_profile
          if(!is.null(profile)) {
            box(
              title = NULL,
              width = 12,
              status = "primary",
              solidHeader = FALSE,
              div(class = "detail-panel",
                  fluidRow(
                    column(4,
                           div(class = "detail-image-container",
                               img(src = profile$image, class = "detail-image")
                           )
                    ),
                    column(8,
                           div(class = "detail-info",
                               h2(profile$name, class = "detail-name"),
                               
                               div(class = "detail-item",
                                   p(strong("NIM: "), profile$nim)
                               ),
                               div(class = "detail-item", 
                                   p(strong("Role: "), profile$role)
                               ),
                               div(class = "detail-item",
                                   p(strong("Email: "), profile$email)
                               ),
                               div(class = "detail-item",
                                   p(strong("Phone: "), profile$phone)
                               ),
                               div(class = "detail-item",
                                   p(strong("Skills: "), profile$skills)
                               ),
                               div(class = "detail-item",
                                   p(strong("Hobby: "), profile$hobby)
                               ),
                               div(class = "detail-item",
                                   p(strong("Motto: "), em(paste0('"', profile$motto, '"')))
                               ),
                               
                               actionButton("btn_back_overview", 
                                            "← Kembali ke Overview", 
                                            class = "back-btn")
                           )
                    )
                  )
              )
            )
          }
        }
    )
  })
  
  # Navigasi
  observeEvent(input$to_dashboard_button, {
    updateTabItems(session, "sidebarMenuid", "dashboard-provinsi")
  })
  
  observeEvent(input$more_info_button, {
    output$more_info_ui <- renderUI({
      div(
        style = "margin-top: 20px; padding: 20px; background: rgba(255,255,255,0.1); border-radius: 10px;",
        h4("Tentang Dashboard", style = "color: white; margin-bottom: 15px;"),
        p("Dashboard ini dikembangkan sebagai bagian dari tugas akhir mata kuliah Komputasi Statistik. 
          Tim kami terdiri dari 6 mahasiswa Politeknik Statistika STIS yang berkomitmen untuk 
          menyajikan analisis data perubahan iklim dan bencana alam di Indonesia secara interaktif dan informatif.",
          style = "color: #f8f9fa; text-align: justify; line-height: 1.6;")
      )
    })
  })
  
  # DOWNLOAD HANDLERS UNTUK LAPORAN
  
  output$download_laporan_provinsi <- downloadHandler(
    # UBAH EKSTENSI FILE DI SINI
    filename = function() {
      paste("Laporan_Dashboard_Provinsi_", input$provinsi_terpilih, "_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "laporan_provinsi_template.Rmd")
      file.copy("templates/laporan_provinsi_template.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        provinsi = input$provinsi_terpilih,
        tahun_awal = input$rentang_tahun_provinsi[1],
        tahun_akhir = input$rentang_tahun_provinsi[2],
        data = provinsi_filter()
      )
      
      # Render laporan (tidak ada yang perlu diubah di sini)
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  # Download Handler untuk Dashboard Bencana
  output$download_laporan_bencana <- downloadHandler(
    filename = function() {
      paste("Laporan_Dashboard_Bencana_", gsub(" ", "_", input$bencana_terpilih), "_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "laporan_bencana.Rmd")
      file.copy("templates/laporan_bencana_template.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        bencana = input$bencana_terpilih,
        tahun_awal = input$rentang_tahun_bencana[1],
        tahun_akhir = input$rentang_tahun_bencana[2],
        data = bencana_filter()
      )
      
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  # Download Handler untuk Peta Bencana
  output$download_laporan_peta_bencana <- downloadHandler(
    filename = function() {
      variabel_nama <- names(Jenis_Bencana)[Jenis_Bencana == input$peta_bencana_terpilih]
      paste("Laporan_Peta_Bencana_", gsub(" ", "_", variabel_nama), "_", input$tahun_peta_bencana, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "laporan_peta_bencana.Rmd")
      file.copy("templates/laporan_peta_bencana_template.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        Jenis_Bencana = input$peta_bencana_terpilih,
        variabel_nama = names(Jenis_Bencana)[Jenis_Bencana == input$peta_bencana_terpilih],
        tahun = input$tahun_peta_bencana,
        data = peta_bencana_filter()
      )
      
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  # Download Handler untuk Peta Iklim
  output$download_laporan_peta_iklim <- downloadHandler(
    filename = function() {
      variabel_nama <- names(variabel_iklim)[variabel_iklim == input$peta_iklim_terpilih]
      paste("Laporan_Peta_Iklim_", gsub(" ", "_", variabel_nama), "_", input$tahun_peta_iklim, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "laporan_peta_iklim.Rmd")
      file.copy("templates/laporan_peta_iklim_template.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        variabel_iklim = input$peta_iklim_terpilih,
        variabel_nama = names(variabel_iklim)[variabel_iklim == input$peta_iklim_terpilih],
        tahun = input$tahun_peta_iklim,
        data = peta_iklim_filter()
      )
      
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  # Download Handler untuk Analisis Korelasi
  output$download_laporan_korelasi <- downloadHandler(
    filename = function() {
      paste("Laporan_Analisis_Korelasi_", input$prov_korelasi, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "laporan_korelasi.Rmd")
      file.copy("templates/laporan_korelasi_template.Rmd", tempReport, overwrite = TRUE)
      
      # Siapkan data untuk korelasi
      dat <- if (input$prov_korelasi == "Indonesia") data_dashboard
      else filter(data_dashboard, provinsi == input$prov_korelasi)
      
      # Apply transformasi
      for (v in input$vars_korelasi) {
        if (v == input$dep_var)  dat[[v]] <- apply_transformation(dat[[v]], input$trans_dep)
        else                     dat[[v]] <- apply_transformation(dat[[v]], input$trans_indep)
      }
      
      params <- list(
        provinsi = input$prov_korelasi,
        variabel = input$vars_korelasi,
        data = dat[, input$vars_korelasi, drop = FALSE],
        trans_dep = input$trans_dep,
        trans_indep = input$trans_indep
      )
      
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
  # Download Handler untuk Analisis Regresi
  output$download_laporan_regresi <- downloadHandler(
    filename = function() {
      paste("Laporan_Analisis_Regresi_", input$prov_var, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "laporan_regresi.Rmd")
      file.copy("templates/laporan_regresi_template.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(
        provinsi = input$prov_var,
        dep_var = input$dep_var,
        indep_vars = input$indep_vars,
        trans_dep = input$trans_dep,
        trans_indep = input$trans_indep,
        model = model_fit(),
        data = filtered_data(),
        assumptions = list(
          homoskedastisitas = assumption_results$homoskedastisitas,
          normalitas = assumption_results$normalitas,
          multikolinearitas = assumption_results$multikolinearitas,
          autokorelasi = assumption_results$autokorelasi
        )
      )
      
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui = ui, server = server)