library(SeminarMatching)

setwd("D:/libraries/SeminarMatching/semapps/stud_de")
main.dir = "../shared"
app = StudSeminarsApp(init.userid = "test", init.password="test", lang="de",main.dir = main.dir)
appReadyToRun(app)
#viewApp(app)
