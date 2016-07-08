library(SeminarMatching)

setwd("D:/libraries/SeminarMatching/semapps/stud_en")
main.dir = "../shared"
app = StudSeminarsApp(init.userid = "test", init.password="test", lang="en",main.dir = main.dir)
appReadyToRun(app)
#viewApp(app)
