library(SeminarMatching)

main.dir = "/srv/sem-shared"
setwd(main.dir)
email.text.fun = function(lop, email,link,...) {
  int = sample.int(1e4,1)
  subject = paste0("Confirm Registration to Seminar Matching (Your University)(",int,")" )
  body = paste0("
Dear Administrator,

you get this email, because you registered to the seminar matching. Follow the link below to confirm your email adress and continue:\n\n", link$url,
"\n
"
  )

  msg = paste0(
"We have send an confirmation email to ", email," from ",lop$smtp$from,
"<br>The email contains a link to confirm your account and generate a password. <br>The subject of the email is: <br><br>",subject,"<br>")
  nlist(subject, body, msg)
}

restore.point.options(display.restore.point = FALSE)
set.storing(FALSE)
host.name = "localhost"
host.name = "172.17.0.1" # docker bridge to host

app = AdminSeminarsApp(
  #init.userid = "test", init.password="test",
  lang="en",
  main.dir = main.dir,
  app.title = "SeminarMatching - Lecturer App",
  app.url = "http://<your-url>:3838/semapps/teacher",
  email.domain = "@<your-university.edu>",
  email.text.fun=email.text.fun,
  smtp = list(from = "seminars@<your-university.edu>",smtp = list(host.name = host.name))
)
appReadyToRun(app)

