library(keira)

converter("misc/45_items.docx")

intro <- "Hallo"
points <- 1

set.seed(3450435)
keira::generate(files=list.files("items", full.names=TRUE),
                n=2,
                title="Alpine Therapieverfahren M01 / Prof. Huber / BSc. Psychologie SS 23 /",
                course = "M99",
                showpoints = TRUE,
                intro = intro,
                points = points,
                date="2023-02-15",
                logo = "./msblogo.png",
                output_dir = "exam"
)



examsMSB::nops_scan(dir = "misc/testsuite/45items/scans_png/")
# Hier müssen wir nun die Gesamtpunktzahl angeben, damit
# wir die korrekte Abbildung von Punkten auf Noten erhalten
totalpoints <- 45
ml <- keira::get_marks_and_labels(totalpoints)

#
#
eval_fun <- examsMSB::exams_eval(partial=TRUE, rule="true", negative=FALSE)
eval_fun <- examsMSB::exams_eval(partial=FALSE)


# Nun werden die Informationen aus den korrekten und falschen
# Antworten (RDS-Datei aus Schritt 1) und die ausgelesenen
# Informationen zusammengeführt.
#
# Es wird eine Datei namens "nops_eval.csv" erzeugt, die
# eine Übersicht der Punkte, Gesamtpunkte und Note pro
# Matrikelnummer enthält.

examsMSB::nops_eval(register=NULL,
                    solutions="misc/testsuite/45items/exam/klausur_M99_2023-02-15.rds",
                    scans="misc/testsuite/45items/scans_png/nops_scan_20240312164657.zip",
                    mark=ml$marks,
                    labels=ml$labels,
                    eval=eval_fun,
                    interactive = TRUE)




keira::grade_report(nops_eval_file = "nops_eval.csv",
                    path_to_scans = "misc/testsuite/45items/scans_png/nops_scan_20240312164657.zip",
                    outfolder = "reports",
             show_points = TRUE,
             show_points_total_max = TRUE,
             points_total_max = 45,
             show_keira_footer = TRUE,
             signature = "signature-brandmaier-blue.png",
             signature_rel_x=.6,
             signature_rel_y=1,
             debug=TRUE)
