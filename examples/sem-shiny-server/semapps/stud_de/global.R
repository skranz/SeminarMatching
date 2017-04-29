library(SeminarMatching)

main.dir = "/srv/sem-shared"
setwd(main.dir)

email.text.fun = function(lop, email,link,...) {
  int = sample.int(1e4,1)
  subject = paste0("Bestätige Anmeldung für Seminarvergabe, Uni ???? (",int,")" )

  body = paste0("
Lieber Studierender,

Sie erhalten diese Email, weil sie sich vor einigen Sekunden als neuer Nutzer für die zentralisierte Seminarvergabe an der Uni ???? registriert haben, oder Ihr Passwort zurücksetzen wollten.

Um Ihr Nutzerkonto zu bestätigen und ein Passwort zu wählen, klicken Sie bitte auf folgenden Link:\n\n", link$url,
"\n

Viele Grüße,

Ihr Seminarvergabe-Team
"
  )

  msg = paste0("Wir haben Ihnen eine Bestätigungsemail an ", email," von ",lop$smtp$from,
               " gesendet.<br>Die Email enthält einen Link um ein Passwort zu generieren und ihr Konto zu aktivieren. <br>Der Titel der Email lautet: <br><br>",subject,"<br>")

  nlist(subject, body, msg)
}

restore.point.options(display.restore.point = FALSE)
set.storing(FALSE)
host.name = "localhost"
host.name = "172.17.0.1" # docker bridge to host

app = StudSeminarsApp(
  #init.userid = "test", init.password="test",
  lang="de",
  main.dir = main.dir,
  app.title = "Seminarvergabe",
  app.url = "http://<your-url>:3838/semapps/stud_de",
  email.domain = "@<your-university.edu>",
  email.text.fun=email.text.fun,
  smtp = list(from = "seminars@<your-university.edu>",smtp = list(host.name = host.name))
)
appReadyToRun(app)

