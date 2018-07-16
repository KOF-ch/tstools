rm(list=ls())

# This Script generates the general VJA Graphs - Marc Anderes

annpct <- function(x){
  
  result <- ((x/stats::lag(x,-1))^frequency(x)-1)*100
  colnames(result) <- colnames(x)
  return(result)
  
}



# Preamble ----------------------------------------------------------------
# Options
options(scipen=999) # No scientific notation

#library(tstools)
devtools::load_all()
library(kofbts)
library(openxlsx)


vja_settings <- read.xlsx("inst/vja.xlsx",
                          colNames = F, rowNames = T)

date_current <- as.yearqtr(paste0(vja_settings["year",1]," Q",vja_settings["quarter",1]))

settings <- list(
  
  vja_current = paste0("vja", format(date_current, "%y%q")),
  vja_previous = paste0("vja", format(date_current-0.25, "%y%q")),
  horizon = as.integer(vja_settings["horizon",1]),
  last_forecast_date = vja_settings["last_date",1],
  cur_forecast_date = vja_settings["cur_date",1],
  last_forecast_date_eng = vja_settings["last_date_eng",1],
  cur_forecast_date_eng = vja_settings["cur_date_eng",1],
  graph_start1 = as.integer(vja_settings["graph_start1",1]),
  graph_start2 = as.integer(vja_settings["graph_start2",1])
)

vja_current <- paste0("vja", format(date_current, "%y%q"))
vja_current <- "vja181"
vja_previous <- paste0("vja", format(date_current-0.25, "%y%q"))
horizon <- as.integer(vja_settings["horizon",1])
last_forecast_date <- vja_settings["last_date",1]
cur_forecast_date <- vja_settings["cur_date",1]
last_forecast_date_eng <- vja_settings["last_date_eng",1]
cur_forecast_date_eng <- vja_settings["cur_date_eng",1]
graph_start1 <- as.integer(vja_settings["graph_start1",1])
graph_start2 <- as.integer(vja_settings["graph_start2",1])




con <- kofDbConnect()



keys_ch <- paste("ch.kof", vja_current, c("gdp","consp","consg","ifix","extot1","imtot1", "iinv1"), sep = ".")
keys_fx <- paste("ch.kof", vja_current, c("chfjpy", "chfeur", "chfusd", "chf_reer"), sep = ".")
keys_bvar <- paste("ch.kof", vja_current, c("bip", "gcland",	"gcbau",	"gchandel",	"gcfinanz",	"gcsonst",	"gcbericht"), sep = ".")
keys_trade <- paste("ch.kof",vja_current,c("exc1i", "wkn", "wact"), sep = ".")

keys <- unique(c(keys_ch, keys_fx, keys_bvar, keys_bvar, keys_trade))
tslist <- readTimeSeries(keys, con, schema = "ext_timeseries")
dbDisconnect(con)

con_prod <- kofDbConnect("prod")
kofbaro <- readTimeSeries('kofbarometer',con_prod,schema = "pblc_timeseries")
dbDisconnect(con_prod)

# vja current hacked cause data seems not to be in postgres yet
vja_current <- "vja181"
graph_start1 <- settings$graph_start1
# Reales BIP und KOF Konjunkturbarometer
ts <- list(window(annpct(na.omit(tslist[[paste("ch.kof",vja_current,"gdp", sep = ".")]])), start= graph_start1, end=c(horizon, 4)), window(kofbaro$kofbarometer, start=graph_start1))

kofbaro_naming <- as.integer(if(substr(vja_current,start = 6, stop=6)==3){paste0("20", as.integer(substr(vja_current,start = 4, stop=5))-1)})
bip_naming <- paste0(substr(vja_current, start=6, stop=6), ". Quartal ", "20", as.integer(substr(vja_current,start = 4, stop=5)))

names(ts) <- c("Reales BIP (annualisierte VQ-Veränderung, in %)", paste0("KOF Konjunkturbarometer (Mittel ", kofbaro_naming-9,"-", kofbaro_naming, "=100), rechte Skala"))

# THEMES BY MARC ####################
# KOF Color Definitions

kof_lines <- c("eth_8_100" = "#007a92",
               "eth_5_60" = "#cc67a7",
               "eth_4_100" = "#72791c",
               "eth_8_60" = "#66b0c2",
               "eth_5_100" = "#91056a",
               "eth_4_60" = "#a9af66",
               "eth_8_20" = "#cce5eb",
               "eth_6_100" = "#6f6f6e",
               "eth_5_30" = "#e6b3d3",
               "schwarz" = "#000000")

kof_bars <- c("eth_8_100" = "#007a92",
              "eth_8_60" = "#66b0c2",
              "eth_8_20" = "#cce5eb",
              "eth_5_30" = "#e6b3d3",  
              "eth_5_60" = "#cc67a7",
              "eth_5_100" = "#91056a",
              "eth_7_100" = "#a8322d",
              "eth_7_50" = "#e19794",
              "eth_6_60" = "#a9a9a8",
              "weiss" = "#ffffff")


# Theme for monthly and quarterly frequency
tt <- init_tsplot_print_theme(output_dim = c(4, 3), title_cex.main = 1.05, subtitle_cex = 1.05)
tt$highlight_window <- T
tt$line_colors <- kof_lines
tt$bar_fill_color <- kof_bars
tt$lty <- c(1,5,2,4,6,5,1,4,2,6)
tt$subtitle_transform <- NULL
tt$highlight_window_end <- c(settings$horizon+4,1)
# tt$highlight_color <- c("eth_8_20" = "#cce5eb")
#tt$use_box <- T
#tt$margins <- c(6.1,2.6,3.1,3.1)
#tt$title_cex.main <- c(1.0)
#tt$subtitle_cex.main <- c(1.0)
#tt$legend_cex <- c(1)
tt$use_box = F
#tt$legend_col <- 2
#tt$lwd <- c(2,3,2,4,2,3,2,4)
#tt$sum_line_lwd <- 2
tt$sum_line_color <- "#000000"

tt_y <- tt_q <- tt_m <- tt
tt$highlight_window <- F
current_edge_m <- as.numeric(paste0("20",(substr(settings$vja_current,4,5)))) + (as.numeric(substr(settings$vja_current,6,6))-1)/4 + 1/6
current_edge_q <- as.numeric(paste0("20",(substr(settings$vja_current,4,5)))) + (as.numeric(substr(settings$vja_current,6,6))-1)/4
current_edge_y <- as.numeric(paste0("20",(substr(settings$vja_current,4,5)))) 
tt_m$highlight_window_start <- c(current_edge_m,1)
tt_q$highlight_window_start <- c(current_edge_q,1)
tt_y$highlight_window_start <- c(current_edge_y,1)

rm(current_edge_m,current_edge_q,current_edge_y)




# Example plots ######################
tt_m$legend_col <- 1
tsplot(ts[1], tsr = ts[2], 
       theme=tt_m,
       left_as_bar = T,
       manual_value_ticks_l = seq(-2,6, by=1),
       manual_value_ticks_r = seq(80,120, by=5),
       plot_title = "Reales BIP und KOF Konjunkturbarometer",
       plot_subtitle = paste0("BIP ab ", bip_naming, ": Schätzung/Prognose KOF"),
       output_format = "pdf",
       filename = "realesBIPundBaro")
tt_m$legend_col <- 2





# Devisenkurse mit Prognose
ts <- list("CHF / EUR" = window(tslist[[paste("ch.kof",vja_current,"chfeur", sep = ".")]],
                                start= graph_start1, end=c(horizon, 12)),
           "CHF / 100 Yen" =window(tslist[[paste("ch.kof",vja_current,"chfjpy", sep = ".")]], start= graph_start1, end=c(horizon, 12)),
           "CHF / USD" =window(tslist[[paste("ch.kof",vja_current,"chfusd", sep = ".")]], start= graph_start1, end=c(horizon, 12)))


#tt_m$margins <- c(6.1,2.6,2.1,3.1) # No subtitle, so graph has to be taller
tsplot(ts,  
       theme=tt_m,
       manual_value_ticks_l = seq(0.6,1.6, by=0.2),
       plot_title = "Devisenkurse mit Prognose",
       output_format = "pdf",
       filename = "DevisenmitPrognose")
#tt_m$margins <- c(6.1,2.6,3.1,3.1)


# Wachstumsbeiträge BIP: Produktionsseitig

ts <- tslist[keys_bvar]
names(ts) <- c("BIP", "Industrie (inkl. Landwirtschaft)", "Baugewerbe/Bau", "Handel, Kommunikation, Transport & Gastgewerbe", 
               "Finanz und sonstige wirtschaftliche Dienstleistungen", "Konsumentennahe Dienstleistungen", "Berichtigungen")

tt_y$sum_as_line <- T
tsplot(ts[2:7],  # Missing: Sum_as_line funktion von Matthias. Falls GDP Line einzeln geplottet wird, gibts zweite Achse
       theme=tt_y,
       left_as_bar = T,
       manual_value_ticks_l = seq(-3,5, by=1),
       plot_title = "Wachstumsbeiträge BIP: Produktionsseitig",
       plot_subtitle = "(in PP des BIP)",
       output_format = "pdf",
       filename = "WachstumsbeiträgeBIPProduktionsseitig")
tt_y$sum_as_line <- F


# Schweizer Warenexporte und ihr Umfeld
# Since first two series have same scale, both are bars (tsplot does not feature plot AND line on the left, both lines have to be on same right axis)
ts <- tslist[paste("ch.kof",vja_current,c("exc1i", "wkn", "wact"), sep = ".")]

ts <- list("Warenexporte in die Industrieländer" = annpct(ts[[1]]), "Nom. Frankenkurs (24 Länder, exportgew., invertiert)" = -annpct(ts[[2]]), 
           "ausl. Wirtschaftsaktivität (Schätzung KOF), rechte Skala" = annpct(ts[[3]]))
ts <- lapply(ts, function(x) window(x, start=graph_start1, end=c(as.integer(paste0("20",substr(vja_previous, start=4, stop=5))),
                                                                 as.integer(substr(vja_previous, start=6, stop=6)))))
tt$legend_col <- 1
tsplot(ts[1:2], tsr=ts[3],
       theme=tt,
       left_as_bar = T,
       group_bar_chart = T,
       manual_value_ticks_l = seq(-50,30, by=10),
       manual_value_ticks_r = seq(-2,6, by=1),
       plot_title = "Schweizer Warenexporte und ihr Umfeld",
       plot_subtitle = "(annualisierte Veränderung gegenüber Vorquartal, in %)",
       output_format = "pdf",
       filename = "WarenexporteundUmfeld")
tt$legend_col <- 2







