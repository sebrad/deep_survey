# load libs
library(magick)
library(pdftools)
library(abind)
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
N <- 1000

# anzahl fragen
n <- length(fragen)

# kreuze bereiche für zufälligkeit
angles <- 0:360 # kreuze drehungen
img_angle <- -5:5 # rotation der fragen 
sizes <- 15:40 # größen der krueuze
m_size <- max(sizes)

# ITERIEREN
MAT <- list()
LABELS <- list()
QUESTION <- list()
iter <- 0
for(i in 1:n){
  fr <- frage_pos[i,]$frage
  ant_info <- antwort_pos[antwort_pos$frage == fr,]
  answ_samp <- sample(ant_info$antw,N, replace = T)
  for(j in 1:N){
    iter <- iter + 1
    print(paste0(i,":", j))
    answ <- answ_samp[j]
    pos_x <- ant_info[ant_info$antw == answ,"antw_pos_x_abs"]-frage_pos[i,"pos_x"]
    pos_y <- ant_info[ant_info$antw == answ,"antw_pos_y_abs"]-frage_pos[i,"pos_y"] 
    #pos_x <- 820-frage_pos[i,"pos_x"]
    #pos_y <- 1562-frage_pos[i,"pos_y"] 
    x <- pos_x + (2*rbinom(1,1,.5)-1) * sample(0:8,1)
    y <- pos_y + (2*rbinom(1,1,.5)-1) * sample(0:8,1)
    angle <- sample(angles, 1)
    bord_x <- (max(sizes) - sample(sizes,1))/2
    bord_y <- (max(sizes) - sample(sizes,1))/2
    kl_kreuz <- image_rotate(image_background(kreuz[[sample(1:klen,1)]], "none"), angle)
    kl_kreuz <- image_scale(image_border(image_scale(kl_kreuz, m_size),  "none", 
                                         paste0(bord_x, "x", bord_y)), m_size)
    img <- image_composite(fragen[[i]], kl_kreuz, offset = paste0("+",x,"+",y)) 
    
    # random rotation and fixed output size
    img <- image_rotate(img, sample(img_angle,1))
    if(ant_info$frage == 1) img <- image_rotate(img, 90)
    img <- image_scale(img, "290x")
    img <- image_crop(img, "290x50")
    img <- image_scale(img, "290x50!")
    
    # to grayscale and normalization
    img <- image_convert(img, type = "grayscale", colorspace = "gray")
    img <- image_normalize(img)
    
    # to numeric
    img_num <- strtoi(image_data(img), base = 16)
    dim(img_num) <- c(290, 50)
    
    #print(image(img_num))
    
    # save to container
    MAT[[iter]] <- img_num
    LABELS[[iter]] <- answ
    QUESTION[[iter]] <- fr
  }
}

# combine MAT to array
# combine to array 
ARR <- do.call(abind,c(MAT,list(along=0)))
rm(MAT)
saveRDS(ARR, "data/03_simulation_data/simulation1_image_array.rds")

# save labels as data.frame
labels <- data.frame(frage = unlist(QUESTION), label = unlist(LABELS))
saveRDS(labels, "data/03_simulation_data/simulation1_label_data_frame.rds")

