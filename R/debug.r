example.lower.case = function() {
  setwd("D:/libraries/SeminarMatching/semapps/shared")
  db.dir = paste0(getwd(),"/db")
  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())
  db = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())

  semester = "SS17"
  students = dbGet(db,"students", params=nlist(semester))
  studprefs = dbGet(db,"studpref", params=nlist(semester))

  df = studprefs %>%
    group_by(userid) %>%
    summarize(num_prefs = n())

  students = left_join(students, df, by="userid") %>%
    mutate(num_prefs = ifelse(is.na(num_prefs),0,num_prefs))

  dup = students %>%
    filter(num_prefs>0) %>%
    mutate(lowid = tolower(userid)) %>%
    group_by(lowid) %>%
    mutate(nid=n()) %>%
    filter(nid>1) %>%
    arrange(lowid,num_prefs) %>%
    ungroup()

  cat(paste0(unique(dup$lowid), collapse=","))


  dup

  out = select(dup, email, num_prefs)

  email = students$email

  length(unique(email))
  length(unique(tolower(email)))

  temail = tolower(email)
  email[duplicated(temail)]
  email[rev(duplicated(rev(temail)))]


  cat(paste0(email[duplicated(temail)], collapse=","))

}
