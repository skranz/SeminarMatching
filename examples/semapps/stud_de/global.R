library(SeminarMatching)

#setwd("D:/libraries/SeminarMatching/semapps/stud_de")

# adapt main.dir to path of shared folder on your server
main.dir = "../shared"
app = StudSeminarsApp(lang="de",main.dir = main.dir)
appReadyToRun(app)
#viewApp(app)
