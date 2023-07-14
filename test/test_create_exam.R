#
# (Schritt 1) Klausur erstellen
#
#
# author: Andreas M. Brandmaier
# contact: andreas.brandmaier@medicalschool-berlin.de
# date: 2023/04
# version: 1.0
#

# zunächst laden wir die beiden benötigten R-Pakete
#
library(examsMSB)
library(keira)

# (1) Word-Klausur in einzelne Frage-Dateien in LaTeX umwandeln
#     Die entstehenden Dateien liegen dann im Unterordner 'items'
keira::converter("misc/testcase-multiple-answers-correct.docx")

# (2) Hinweistext erstellen und nach LaTeX konvertieren
intro <- "Hinweistext: Für jede Frage mit einer richtigen Antwort erhalten Sie 1,5 Punkte. Für jede Frage mit zwei richtigen Antworten erhalten Sie 3 Punkte. Es zählen nur vollständig richtig beantwortete Fragen. Insgesamt besteht die Klausur aus 42 Fragen (13 Fragen mit einer richtigen Antwort; 29 Fragen mit zwei richtigen Antworten), so dass max. eine Punktzahl von 106,5 erreicht werden kann. "
intro <- keira::convert_latex_simple(intro)

# (3) Anzahl der Punkte pro Aufgabe festlegen

# (3a) Möglichkeit 1: Alle Aufgaben erhalten dieselbe Punktzahl
points <- 1

# (3b) Möglichkeit 2: Wir vergeben händisch unterschiedliche Punkte pro
#        Aufgabe
#points <- c(1,1,2,2,1,1,10)

# (3c) Möglichkeit 3: Wir vergeben unterschiedliche Punktzahlen
#       für jeweils single-choice und multi-choice Aufgaben
#points <- keira::extract_points(list.files("items", full.names=TRUE),
#                                schoice=1.5, mchoice=3)


# (4) Wir setzen einen Zufalls-Seed, damit die Verwürfelung der Distraktoren
#       reproduzierbar ist
#
# n:        Anzahl der permutierten Varianten (n=1 für nur eine Version, N=2 für A/B,...)
# title:    Titel der Klausur - muss Modulbezeichnung enthalten
# course:   Nur die Modulbezeichnung
# showpoints: TRUE oder FALSE, sollen bei jedem Item
#              die erreichbaren Punkte angezeigt werden
# intro:
# points:
# date:     Datum der Klausur
#
#
# Die Klausuren werden im Unterordner "exam" abgelegt
# Es entsteht die gewünschte Anzahl an PDF-Dateien, die
# jeweils eine der n Klausurvarianten entsprechen
#
# Wichtig: Es entsteht ausserdem eine RDS-Datei, die
# alle Informationen zu den richtigen&falschen Antworten
# enthält. Diese Datei wird später bei der Korrektur
# benötigt und muss unbedingt sicher aufbewahrt werden.

set.seed(3450435)
keira::generate(files=list.files("items", full.names=TRUE),
                n=2,
                title="M99 Alpine Therapieverfahren",
                course = "M99",
                showpoints = TRUE,
                intro = intro,
                points = points,
                date="2023-02-15",
                logo = "./msblogo.png",
                output_dir = "exam"
)
