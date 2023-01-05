library(keira)

dir.create("temp")
converter("misc/Testdoc.docx")

# this is supposed to fail!
converter("misc/testcase-answer-missing.docx")

# this is supposed to work
converter("misc/testcase-multiple-answers-correct.docx")


converter("../../../Lehre/M25-P-WS-2022/9.Klausur/Klausurfragen-Pool2.docx", debug=FALSE,
          include_tags = "#probeklausur", exclude_tags=c())
