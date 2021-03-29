#############################################################################
#     Shiny apps for displaying summary statistics of 
#     Indonesian Corruption Database ver.5.0
#     Author: Putu Sanjiwacika Wibisana
#
#############################################################################


library(shiny)
library(shinyWidgets)
library(stats)
library(dplyr)
library(rlang)
library(ggplot2)
library(ggthemes)
library(arsenal)

#### Cleaner ####

df_raw <- read.csv("Database Korupsi Final2.csv", stringsAsFactors = TRUE) %>%
  select(-one_of(c("Pekerjaan"))) %>%
  rename(Pekerjaan = Pekerjaan_class) %>%
  mutate(Pulau = ifelse(Provinsi.x %in% c("Provinsi Aceh",
                                          "Provinsi Sumatera Utara",
                                          "Provinsi Sumatera Barat",
                                          "Provinsi Riau",
                                          "Provinsi Kepulauan Riau",
                                          "Provinsi Sumatera Selatan",
                                          "Provinsi Kepulauan Bangka Belitung",
                                          "Provinsi Jambi",
                                          "Provinsi Lampung",
                                          "Provinsi Bengkulu"),
                        "Sumatera",
                        ifelse(Provinsi.x %in% c("Provinsi DKI Jakarta"),
                               "DKI Jakarta",
                               ifelse(Provinsi.x %in% c("Provinsi Jawa Barat",
                                                        "Provinsi Banten",
                                                        "Provinsi Jawa Tengah",
                                                        "Provinsi Daerah Istimewa Yogyakarta",
                                                        "Provinsi Jawa Timur"),
                                      "Jawa non-DKI",
                                      ifelse(Provinsi.x %in% c("Provinsi Bali",
                                                               "Provinsi NTB",
                                                               "Provinsi NTT"),
                                             "Bali-Nusra",
                                             ifelse(Provinsi.x %in% c("Provinsi Kalimantan Barat",
                                                                      "Provinsi Kalimantan Tengah",
                                                                      "Provinsi Kalimantan Selatan",
                                                                      "Provinsi Kalimantan Timur",
                                                                      "Provinsi Kalimantan Utara"),
                                                    "Kalimantan",
                                                    ifelse(Provinsi.x %in% c("Provinsi Sulawesi Barat",
                                                                             "Provinsi Sulawesi Tengah",
                                                                             "Provinsi Sulawesi Selatan",
                                                                             "Provinsi Sulawesi Tenggara",
                                                                             "Provinsi Sulawesi Utara",
                                                                             "Provinsi Gorontalo"),
                                                           "Sulawesi",
                                                           ifelse(Provinsi.x %in% c("Provinsi Maluku",
                                                                                    "Provinsi Maluku Utara"),
                                                                  "Maluku","Papua")))))))) %>%
  mutate(Kelompok_Usia = ifelse(Umur<25, "Di bawah 25 tahun",
                               ifelse(Umur<35, "25-34 tahun",
                                      ifelse(Umur<45, "35-44 tahun",
                                             ifelse(Umur<55, "45-54 tahun",
                                                    ifelse(Umur<65, "55-64 tahun","65 tahun ke atas")))))) %>%
  mutate(Kelompok_Kerugian_Negara = ifelse(Kerugian_Negara<200000000, "Paling Ringan (< 200 juta)",
                                           ifelse(Kerugian_Negara<1000000000, "Ringan (200 juta ~ 1 M)",
                                                  ifelse(Kerugian_Negara<25000000000, "Sedang (1M ~ 25M)","Berat (>25M)"))))

df <- read.csv("Database Korupsi Final2.csv", stringsAsFactors = TRUE) %>%
  select(one_of(c("Agama",
                  "Jenis_Kelamin",
                  "Umur",
                  "Pekerjaan_class",
                  "Provinsi.x",
                  "Kerugian_Negara",
                  "Tuntutan_Durasi_Penjara_bulan",
                  "Tuntutan_Denda",
                  "Tuntutan_Subsidair_Denda_bulan",
                  "Tuntutan_Uang_Pengganti",
                  "Tuntutan_Subsidair_Uang_Pengganti_bulan",
                  "PN_Durasi_penjara_bulan",
                  "PN_Denda",
                  "PN_Subsidair_Denda_bulan",
                  "PN_Uang_Pengganti",
                  "PN_Subsidair_Uang_Pengganti_bulan",
                  "PT_Durasi_penjara_bulan",
                  "PT_Denda",
                  "PT_Subsidair_Denda_bulan",
                  "PT_Uang_Pengganti",
                  "PT_Subsidair_Uang_Pengganti_bulan",
                  "MA_Durasi_penjara_bulan",
                  "MA_Denda",
                  "MA_Subsidair_Denda_bulan",
                  "MA_Uang_Pengganti",
                  "MA_Subsidair_Uang_Pengganti_bulan",
                  "PK_Durasi_penjara_bulan",
                  "PK_Denda",
                  "PK_Subsidair_Denda_bulan",
                  "PK_Uang_Pengganti",
                  "PK_Subsidair_Uang_Pengganti_bulan",
                  "Final_Durasi_penjara",
                  "Final_Denda",
                  "Final_Subsidair_Denda",
                  "Final_Uang_Pengganti",
                  "Final_Subsidair_Uang_Pengganti"))) %>%
  mutate(Pulau = ifelse(Provinsi.x %in% c("Provinsi Aceh",
                                          "Provinsi Sumatera Utara",
                                          "Provinsi Sumatera Barat",
                                          "Provinsi Riau",
                                          "Provinsi Kepulauan Riau",
                                          "Provinsi Sumatera Selatan",
                                          "Provinsi Kepulauan Bangka Belitung",
                                          "Provinsi Jambi",
                                          "Provinsi Lampung",
                                          "Provinsi Bengkulu"),
                        "Sumatera",
                        ifelse(Provinsi.x %in% c("Provinsi DKI Jakarta"),
                               "DKI Jakarta",
                               ifelse(Provinsi.x %in% c("Provinsi Jawa Barat",
                                                        "Provinsi Banten",
                                                        "Provinsi Jawa Tengah",
                                                        "Provinsi Daerah Istimewa Yogyakarta",
                                                        "Provinsi Jawa Timur"),
                                      "Jawa non-DKI",
                                      ifelse(Provinsi.x %in% c("Provinsi Bali",
                                                               "Provinsi NTB",
                                                               "Provinsi NTT"),
                                             "Bali-Nusra",
                                             ifelse(Provinsi.x %in% c("Provinsi Kalimantan Barat",
                                                                      "Provinsi Kalimantan Tengah",
                                                                      "Provinsi Kalimantan Selatan",
                                                                      "Provinsi Kalimantan Timur",
                                                                      "Provinsi Kalimantan Utara"),
                                                    "Kalimantan",
                                                    ifelse(Provinsi.x %in% c("Provinsi Sulawesi Barat",
                                                                             "Provinsi Sulawesi Tengah",
                                                                             "Provinsi Sulawesi Selatan",
                                                                             "Provinsi Sulawesi Tenggara",
                                                                             "Provinsi Sulawesi Utara",
                                                                             "Provinsi Gorontalo"),
                                                           "Sulawesi",
                                                           ifelse(Provinsi.x %in% c("Provinsi Maluku",
                                                                                    "Provinsi Maluku Utara"),
                                                                  "Maluku","Papua")))))))) %>%
  mutate(Kelompok_Usia = ifelse(Umur<25, "Di bawah 25 tahun",
                               ifelse(Umur<35, "25-34 tahun",
                                      ifelse(Umur<45, "35-44 tahun",
                                             ifelse(Umur<55, "45-54 tahun",
                                                    ifelse(Umur<65, "55-64 tahun","65 tahun ke atas")))))) %>%
  mutate(Kelompok_Kerugian_Negara = ifelse(Kerugian_Negara<100000000, "Kecil (< 100 juta)",
                                           ifelse(Kerugian_Negara<1000000000, "Sedang (100 juta ~ 1 M)",
                                                  ifelse(Kerugian_Negara<25000000000, "Besar (1M ~ 25M)","Kakap (>25M)")))) %>%
  rename(Provinsi = Provinsi.x,
         Pekerjaan = Pekerjaan_class)

#### Main Code ####

names(df) <- gsub("_"," ", names(df))
names(df)

categorical_vars <- c("Jenis Kelamin","Kelompok Usia","Kelompok Kerugian Negara","Pekerjaan","Pulau")
numeric_vars     <- names(df)[!(names(df) %in% c(categorical_vars, "Provinsi", "Agama"))]

ui <- fluidPage(
  titlePanel("Stats Explorer - Database Korupsi Wave 5.0"),
  tabsetPanel(

    tabPanel("Statistik Deskriptif", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 selectInput("tab_depvar", "Variabel Angka", numeric_vars, selected = "Final_Biaya_Perkara"),
                 selectInput("tab_catvar", "Variabel Kategori", categorical_vars, selected = "Jenis_Kelamin")),
               mainPanel(
                 tableOutput("table")),
             ),),
    
    tabPanel("Bar Charts", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 selectInput("bar_depvar", "Variabel Angka", numeric_vars, selected = "Final_Biaya_Perkara"),
                 selectInput("bar_catvar", "Variabel Kategori", categorical_vars, selected = "Jenis_Kelamin")),
               mainPanel(
                 plotOutput("barplot")),
             ),),

    tabPanel("Scatterplot", fluid=T,
             sidebarLayout(
               sidebarPanel(
                 selectInput("scp_xvar", "Variabel Sumbu X", numeric_vars, selected = "Kerugian Negara"),
                 selectInput("scp_yvar", "Variabel Sumbu Y", numeric_vars, selected = "Final Uang Pengganti"),
                 materialSwitch(inputId = "lm_toggle", label = "Linear Fit?", status = "default")),
               mainPanel(
                 plotOutput("scatterplot")),
             ),)
  
  )
)

server <- function(input, output) {
  
  output$table <- renderTable({
    
    mycontrols  <- tableby.control(test=FALSE, total=FALSE,
                                   numeric.test="kwt", cat.test="chisq",
                                   numeric.stats=c("meansd", "median"),
                                   stats.labels=list(meansd='Mean (SD)', median='Median'))
    
    validate(need(input$tab_depvar,''),
             need(input$tab_catvar,''))
    
    summary(tableby(as.formula(paste0(gsub(" ","_",input$tab_catvar),"~",gsub(" ","_",input$tab_depvar))),
                    data=df_raw,
                    control=mycontrols),
            text=TRUE)
    
  })

  output$barplot <- renderPlot({
    
    df_used <- df %>%
      group_by(!!sym(input$bar_catvar)) %>%
      summarise(y = mean(!!sym(input$bar_depvar), na.rm=T)) %>%
      rename(x = !!sym(input$bar_catvar)) %>%
      filter(x!="")
    
    p <- ggplot(df_used, aes(y=y, x=x)) + 
      geom_bar(stat="identity", aes(fill=x)) + 
      theme_classic() + theme(axis.text.x = element_text(angle = 90)) +
      xlab(paste0(input$bar_catvar)) + ylab(paste0("Rata-rata ",input$bar_depvar)) + 
      labs(fill = element_blank())
    
    print(p)
    
  })
  
  output$scatterplot <- renderPlot({
    
    p <- ggplot(df, aes(x=!!sym(input$scp_xvar), y=!!sym(input$scp_yvar))) + 
      geom_point()
      
    if(input$lm_toggle == TRUE){
      p <- p + geom_smooth(method = "lm")
    }
    
    print(p)
    
  })
}

shinyApp(ui = ui, server = server)


#output$table <- renderTable(prop.table(table(gss2012[, input$row],
#                                             gss2012[, input$column])));