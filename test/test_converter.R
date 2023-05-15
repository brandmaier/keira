#library(keira)

dir.create("temp")
converter("misc/Testdoc.docx")

# this is supposed to fail!
converter("misc/testcase-answer-missing.docx")

# this is supposed to work
converter("misc/testcase-multiple-answers-correct.docx")


converter("../../../Lehre/M25-P-WS-2022/9.Klausur/Klausurfragen-Pool2.docx", debug=FALSE,
          include_tags = "#probeklausur", exclude_tags=c())


scanExam("temp_nops_scan/demo_nops_pdf.zip")

evaluate("temp_nops_scan/output.zip", solutions = "demo_nops_pdf/keira-klausur.rds",
         rule="simple")
         #labels=c("1,0","1,3","1,7","2,0","2,3","2,7","3,0","3,3","3,7","4,0","n.b."),
         #marks=seq(0.5, 1, length.out=11 )[1:10])

nops_eval(solutions = "demo_nops_pdf/keira-klausur.rds",
          scans = "temp_nops_scan/output.zip",register = NULL)


converter("~/../Desktop/Klausurfragen_Merkl-MassmannWS2022_2023_mit ListenabsatzKORR_ab.docx",debug = TRUE)
