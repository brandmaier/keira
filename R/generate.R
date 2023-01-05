#'
#'
#' @param files
#' @param n
#' @param title
#' @param name
#'
#'

generate <- function(files, n = 1, title = "", name="keira-klausur", course="M99",
                     showpoints = TRUE, points = NULL) {

if (is.null(points)) {
  points <- rep(1, length(files))
}

ex1 <- exams2nops(files, n = n,
                  dir = "demo_nops_pdf", name = name,
                  #date = "2022-02-29",
                  points = points, # Immer 1 Punkt pro MC-Frage
                  showpoints = showpoints,
                  samepage = TRUE, # questions should stay on the same page
                  duplex = FALSE,language = "de",
                  institution = "MSB Medical School Berlin",
                  #title = "",
                  course = course,
                  reglength = 12, # Laenge der Matrikelnummer
                  logo = "C:/Users/andreas.brandmaier/Documents/msblogo.png",
                  title = title,
                  twocolumn = FALSE,   # Die Fragen werden in einer einzigen Spalte arrangiert
                  intro = paste0(c(
                    "Hinweise: \\\\",
                    "\\begin{itemize}",
                    "\\item Die Klausur besteht aus ",length(files)," geschlossenen Fragen mit vorgegebenen Antwortm\\\"oglichkeiten (jeweils eine Antwort ist richtig) ",
                    "\\item Bitte pr\\\"ufen Sie vor der Abgabe, dass Sie alle Fragen auf dem Deckblatt beantwortet haben. Nur die Antworten auf dem Deckblatt werden ber\\\"ucksichtigt.",
                    "\\item Viel Erfolg!",
                    "\\end{itemize}",
                    "\\vspace{0.5cm}"),sep="",collapse = ""),
                  verbose=FALSE,
                  blank = 0, usepackage=c("color")
)


}
