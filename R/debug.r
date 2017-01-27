example.lower.case = function() {
  setwd("D:/libraries/SeminarMatching/semapps/shared")
  db.dir = paste0(getwd(),"/db")
  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())
  db = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())

  semester = "SS17"
  students = dbGet(db,"students", params=nlist(semester))

  email = students$email

  length(unique(email))
  length(unique(tolower(email)))

  temail = tolower(email)
  email[duplicated(temail)]
  email[rev(duplicated(rev(temail)))]


  cat(paste0(email[duplicated(temail)], collapse=","))

}
