# Function will be called at night each day and performs
# automatic actions like running a seminar matchings if it is due

get.semdb = function(db.dir = paste0(main.dir,"/db"), main.dir = getwd())  {
  restore.point("get.semdb")
  dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())
}

get.current.admin = function(semester=get.default.semester(db=semdb, schemas=schemas), admin=NULL,  schemas = load.and.init.schemas(paste0(schema.dir, "/semdb.yaml")), schema.dir = paste0(main.dir,"/schema"), semdb = get.semdb(main.dir=main.dir), main.dir=getwd()) {

  restore.point("get.current.admin")
  if (is.null(semester)) return(NULL)
  admin = dbGet(semdb,"admin", nlist(semester),schema = schemas$admin)
  if (is.null(admin)) return(NULL)
  admin = as.list(admin)
  init.se.admin(admin)

}

examples = function() {
  setwd("D:/libraries/SeminarMatching/semapps/shared")
  db.dir = paste0(getwd(),"/db")
  yaml.dir = paste0(getwd(),"/yaml")
  n = 30
  semester = "SS17"

  delete.random.students(db.dir=db.dir, semester=semester)

  res = draw.random.students(n=n,semester=semester,insert.into.db = TRUE)


  run.seminar.tasks("round1")
  run.auto.tasks()
}

# Run seminar matching tasks that have been scheduled today or before today
run.auto.tasks = function(main.dir = getwd(), log.file=paste0(main.dir,"/log/auto_run.log")) {
  restore.point("run.auto.tasks")

  admin = get.current.admin(main.dir=main.dir)
  if (is.null(admin)) return()

  tasks = find.auto.tasks(admin=admin, main.dir=main.dir)
  run.seminar.tasks(tasks=tasks, admin=admin, main.dir=main.dir, log.file=log.file)
}

append.log = function(x, log.file=paste0(getwd(),"/log/auto_run.log")) {
  restore.point("append.log")

  try({
    con <- file(log.file,open = "at")
    write(paste0(c(as.character(Sys.time()),x),collapse=","),con)
  })
  try(close(con))
}

run.seminar.tasks = function(tasks, admin=get.current.admin(main.dir=main.dir), main.dir = getwd(), log.file=paste0(main.dir,"/log/auto_run.log")) {
  restore.point("run.seminar.tasks")
  dirs = make.dirs(main.dir)

  if ("round1" %in% tasks) {
    perform.matching(round=1,semester=admin$semester, dirs=dirs)
    append.log(c("matching1",admin$semester), log.file)
  }
  if ("round2" %in% tasks) {
    perform.matching(round=2,semester=admin$semester, dirs=dirs)
    append.log(c("matching2",admin$semester), log.file)
  }

}

make.dirs = function(main.dir=getwd()) {
  list(
    main.dir = main.dir,
    schema.dir = paste0(main.dir,"/schema"),
    yaml.dir = paste0(main.dir,"/yaml"),
    db.dir = paste0(main.dir,"/db"),
    rmd.dir = paste0(main.dir,"/rmd"),
    reports.dir = paste0(main.dir,"/reports"),
    log.dir = paste0(main.dir,"/log")
  )
}

find.auto.tasks = function(admin=NULL,main.dir = getwd()) {

  restore.point("find.auto.tasks")
  if (is.null(admin))
    admin = get.current.admin(main.dir=main.dir)

  if (is.null(admin)) return(NULL)

  tasks = NULL

  today = as.Date(Sys.time())
  if (is.na(admin$round1_done_date) & isTRUE(today >= admin$round1_date))
    tasks = c(tasks,"round1")

  if (is.na(admin$round2_done_date) & isTRUE(today >= admin$round2_date))
    tasks = c(tasks,"round2")

  tasks
}
