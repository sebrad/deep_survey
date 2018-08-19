# load libs
library(magick)
library(pdftools)
library(abind)
library(parallel)
library(foreach)
library(doParallel)
source("R/function.r")

# kreuze einlesen
kreuz <- read_kreuze()
klen <- length(kreuz)

# fragebogen info einlesen
frage_pos <- read.csv("data/01_fragebogen/frage_positionen.csv")
antwort_pos <- read.csv2("data/01_fragebogen/antwort_positionen.csv")

# fragebogen einlesen
fragebogen <- image_read_pdf('data/01_fragebogen/2017 08 Fragebogen_Schmerz_v4_GK.pdf', density = 150)

# fragen extrahieren
fragen <- fragen_extrahieren(frage_positionen = frage_pos, fragebogen = fragebogen)

# ---------------------------------------
# fragen ankreuzen simulieren
# ---------------------------------------

# anzahl fragebögen
N <- 3000 

# anzahl fragen
n <- length(fragen)

# kreuze bereiche für zufälligkeit
angles <- 0:360 # kreuze drehungen
img_angle <- -5:5 # rotation der fragen 
sizes <- 15:40 # größen der krueuze
m_size <- max(sizes)

# Fragen und Kreuze in Array umwandeln
# Dies ist nötig, damit die Objekte an foreach übergeben werden können
fragen_array <- lapply(fragen, function(x) magick_to_array(image = x, channels = 4))
kreuze_array <- lapply(kreuz, function(x) magick_to_array(image = x, channels = 4))

# INIT PARALLEL
cl<-makeCluster(4, outfile = "")
registerDoParallel(cl)

res <- foreach_fragen_ausfuellen(frage_pos = frage_pos, 
                                      antwort_pos = antwort_pos, 
                                      fragen_array = fragen_array, N = N, n = n,
                                      angles = angles, img_angle = img_angle,
                                      sizes = sizes, kreuze_array = kreuze_array)

stopCluster(cl)

# UNLIST
ARR <- unlist(lapply(res, function(x) x$mat), recursive = F)
QUESTION <- unlist(lapply(res, function(x) x$question), recursive = F)
LABELS <- unlist(lapply(res, function(x) x$labs), recursive = F)

# combine ARR to array ans save
ARR <- do.call(abind,c(ARR,list(along=0)))
saveRDS(ARR, "data/03_simulation_data/simulation1_image_array.rds")

# save labels as data.frame
labels <- data.frame(frage = unlist(QUESTION), label = unlist(LABELS))
saveRDS(labels, "data/03_simulation_data/simulation1_label_data_frame.rds")














































































































































