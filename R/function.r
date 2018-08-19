
#' kreuze einlesen
#'
#' @description 
#' Liest kreuze aus angegebenem Verzeichnis ein. Benötigt Library "magick"
#' 
#' @param kreuz_path (default: "data/02_muster/01_kreuze/")
#'
#' @return liste mit kreuzen
#' @export
read_kreuze <- function(kreuz_path = "data/02_muster/01_kreuze/") {
  kreuz_files <- list.files(kreuz_path)
  kreuz_files <- kreuz_files[grep(".png", kreuz_files)]
  kreuz <- list()
  for(i in kreuz_files)
    kreuz[[i]] <- c(image_read(paste0(kreuz_path, i)))
  return(kreuz)
}


#' Fragen extrahieren
#'
#' @description Funktion extrahiert Fragen und speichert Fragen in Liste
#'
#' @param frage_positionen data.frame mit positionen der fragen mit den spalten "frage" "seite" "pos_x" "pos_y" "dim_x" "dim_y"
#' @param fragebogen image_magick-datei mit geladenem fragebogen
#'
#' @return
#' @export
#'
fragen_extrahieren <- function(frage_positionen = frage_pos, fragebogen = fragebogen) {
  fragen <- list()
  for(i in seq_along(frage_pos$frage)){
    pos <- frage_pos[i,]
    seite <- fragebogen[pos$seite]
    fr_dim <- paste0(pos$dim_x, "x", pos$dim_y, "+", pos$pos_x, "+", pos$pos_y)
    fragen[[i]] <- image_crop(seite, fr_dim)
  }
  return(fragen)
}


#' foreach_fragen_ausfuellen
#'
#' @param frage_pos data.frame mit positionen der fragen
#' @param antwort_pos data.frame mit positionen der antworten
#' @param fragen magick-objekt mit bild-dateinen mit fragen
#' @param N anzahl fragebögen für simulation
#' @param n anzahl fragen auf fragebogen
#' @param angles zufällige rotationen der kreuze
#' @param img_angle zufällige bildrotation
#' @param sizes größenaenderung??
#' @param kreuz objekt mit kreuzen für ausfüllen
#'
#' @return liste mit bild-matrizen, frage-labels und fragen-bezeichnungen
#' @export
#'
foreach_fragen_ausfuellen <- function(frage_pos = frage_pos, 
                                      antwort_pos = antwort_pos, 
                                      fragen_array = fragen_array, N = N, n = n,
                                      angles = angles, img_angle = img_angle,
                                      sizes = sizes, kreuze_array = kreuze_array){
  m_size = max(sizes)
  klen = length(kreuz)
  
  foreach(i = 1:n, 
          .export = c("frage_pos", "antwort_pos",
                      "fragen_array", "N", "angles",
                      "img_angle", "sizes", "kreuze_array",
                      "m_size", "klen"),
          .packages = c("magick"))  %dopar% {
            
            
            # FRAGEN und KREUZE zu bildern transformieren
            if(!all(sapply(fragen_array, is.array))) stop("fragen_array has to be list of image_arrays")
            fragen <- lapply(fragen_array, image_read)
            
            if(!all(sapply(kreuze_array, is.array))) stop("kreuze_array has to be list of image_arrays")
            kreuz <- lapply(kreuze_array, image_read)
            
            # START PARALLEL ---------------------------------------------------------------
            fr <- frage_pos[i,]$frage
            ant_info <- antwort_pos[antwort_pos$frage == fr,]
            answ_samp <- sample(ant_info$antw,N, replace = T)
            fra <- fragen[[i]]
            print(paste0(i))
            
            # CONTAINER
            MAT <- list()
            LABELS <- list()
            QUESTION <- list()
            for(j in 1:N){
              answ <- answ_samp[j]
              pos_x <- ant_info[ant_info$antw == answ,"antw_pos_x_abs"]-frage_pos[i,"pos_x"]
              pos_y <- ant_info[ant_info$antw == answ,"antw_pos_y_abs"]-frage_pos[i,"pos_y"] 
              x <- pos_x + (2*rbinom(1,1,.5)-1) * sample(0:8,1)
              y <- pos_y + (2*rbinom(1,1,.5)-1) * sample(0:8,1)
              angle <- sample(angles, 1)
              bord_x <- (max(sizes) - sample(sizes,1))/2
              bord_y <- (max(sizes) - sample(sizes,1))/2
              kl_kreuz <- image_rotate(image_background(kreuz[[sample(1:klen,1)]], "none"), angle)
              kl_kreuz <- image_scale(image_border(image_scale(kl_kreuz, m_size),  "none", 
                                                   paste0(bord_x, "x", bord_y)), m_size)
              img <- image_composite(fra, kl_kreuz, offset = paste0("+",x,"+",y)) 
              
              # random rotation and fixed output size
              img <- image_rotate(img, sample(img_angle,1))
              if(ant_info$frage[1] == 11) img <- image_rotate(img, 90)
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
              
              # return array, labels and question
              # save to container
              MAT[[j]] <- img_num
              LABELS[[j]] <- answ
              QUESTION[[j]] <- fr
            }
            
            list(mat = MAT, labs = LABELS, question = QUESTION)
            # END PARALLEL  ---------------------------------------------------------------
          }
}




#' magick_to_array
#'
#' @description Transform ImageMagick-Object to Array. This is helpful when one wants to perform computation in parallel since the ImageMagick-Pointer cannot be shared betweed multiple instances of R.
#' 
#' @param image ImageMagick-Object
#' @param channels Amount of Channels (For png use 4 (default))
#'
#' @return array
#' @export
#'
magick_to_array <- function(image = fragen[[1]], channels = 4){
  tmp <- as.matrix(image_data(image))
  dim(tmp) <- c(channels, image_info(image)[,"width"], image_info(image)[,"height"]) 
  return(tmp)
}