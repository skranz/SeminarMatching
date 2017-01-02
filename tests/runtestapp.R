library(SeminarMatching)
setwd("D:/libraries/SeminarMatching/testapps/shared")
app = StudSeminarsApp(lang="de")
port = 4646
app.url = "127.0.0.1:4646"
viewApp(app, port=port)
