
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