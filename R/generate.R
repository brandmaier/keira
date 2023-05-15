#'
#'
#' @param files
#' @param n
#' @param title
#' @param name
#'
#'

generate <- function(files, n = 1, title = "", name="", course="M99",
                     showpoints = TRUE, intro = "(default)", points = NULL,
                     date = NULL, solution = FALSE, logo=NULL,
                     institution=NULL, output_dir="exam") {

if (is.null(points)) {
  points <- rep(1, length(files))
} else {
  if (length(points)==1) {
      points <- rep(points, length(files))
  }
}

if (is.null(logo)) {
  logo<-here::here("msblogo.png")
}

  if (is.null(date)) {
    date <- Sys.Date()
  }

  if (is.null(institution)) {
    institution <- "MSB Medical School Berlin"
  }

  if (name=="") {
    name <- paste0("klausur_",course,"_",date,sep="",collapse="")
  }

  if (intro=="(default)") {
    intro <- paste0(c(
      "Hinweise: \\\\",
      "\\begin{itemize}",
      "\\item Die Klausur besteht aus ",length(files)," geschlossenen Fragen mit vorgegebenen Antwortm\\\"oglichkeiten (jeweils eine Antwort ist richtig) ",
      "\\item Bitte pr\\\"ufen Sie vor der Abgabe, dass Sie alle Fragen auf dem Deckblatt beantwortet haben. Nur die Antworten auf dem Deckblatt werden ber\\\"ucksichtigt.",
      "\\item Viel Erfolg!",
      "\\end{itemize}",
      "\\vspace{0.5cm}"))
  }

ex1 <- examsMSB::exams2nops(files, n = n,
                  date = date,
                  dir = output_dir, name = name,
                  #date = "2022-02-29",
                  points = points, # Immer 1 Punkt pro MC-Frage
                  showpoints = showpoints,
                  samepage = TRUE, # questions should stay on the same page
                  duplex = FALSE,language = "de",
                  institution = institution,
                  #title = "",
                  course = course,
                  reglength = 12, # Laenge der Matrikelnummer
                  logo = logo,
                  title = title,
                  twocolumn = FALSE,   # Die Fragen werden in einer einzigen Spalte arrangiert
                  intro = intro,
                  #sep="",collapse = ""),
                  verbose=FALSE,
                  blank = 0,
                  usepackage=c("color"),
                  solution = solution
)


}
