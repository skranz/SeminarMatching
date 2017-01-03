library(SeminarMatching)

# adapt main.dir to path of shared folder on your server
main.dir = "../shared"

app = AdminSeminarsApp(main.dir = main.dir,lang="en")
appReadyToRun(app)
#viewApp(app)
