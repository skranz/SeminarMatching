library(SeminarMatching)

setwd("D:/libraries/SeminarMatching/semapps/admin")
main.dir = "../shared"
restore.point.options(display.restore.point = !TRUE)

app = AdminSeminarsApp(main.dir = main.dir, init.userid = "test", init.password="test", lang="en")
appReadyToRun(app)
#viewApp(app)
