examples.make.test.semester = function() {
  setwd("D:/libraries/SeminarMatching/semapps/shared")
  reset.semester(semester = "WS1920", rounds=1, reset.random=FALSE)
}


reset.semester = function(semester = get.default.semester(db=semdb, schemas = get.db.schema(semdb)), rounds=0, reset.random=FALSE, main.dir = getwd(), semdb = get.semdb(main.dir=main.dir)) {
  restore.point("reset.semester")
  setwd(main.dir)

  assign = dbGet(semdb,"assign",list(semester=semester))
  matchings=  dbGet(semdb,"matchings",list(semester=semester))
  studpref =  dbGet(semdb,"studpref",list(semester=semester))
  students =  dbGet(semdb,"students",list(semester=semester))


  if (rounds == 0) {
    assign = assign[c(),]
    matchings = matchings[c(),]
    studpref = filter(studpref, round==1)
  } else if (rounds == 1) {
    assign = filter(assign, assign_method=="r1")
    matchings = filter(matchings, round==1)
  }

  if (reset.random) {
    students$random_points = runif(NROW(students), 0, 10)
  }


  dbDelete(semdb,"assign",list(semester=semester))
  dbDelete(semdb,"matchings",list(semester=semester))
  dbDelete(semdb,"studpref",list(semester=semester))
  dbDelete(semdb,"students",list(semester=semester))
  dbDelete(semdb,"manual",list(semester=semester))

  dbInsert(semdb,"assign",assign)
  dbInsert(semdb,"matchings",matchings)
  dbInsert(semdb,"students",students)
  dbInsert(semdb,"studpref",studpref)

  if (rounds == 0) {
    dbUpdate(semdb, "seminars",list(filled_slots=0),list(semester=semester))
  }

  sem = semester
  admins = dbGet(semdb, "admin")
  ca = filter(admins, semester==sem)
  ca$default_start_date = Sys.Date()-2
  ca$stud_start_date = Sys.Date()-2

  if (rounds==0) {
    ca$round1_date = Sys.Date()
    ca$round2_date = Sys.Date()+4
    ca$round1_done_date = NA
    ca$round2_done_date = NA
    ca$rounds_done = 0
  } else if (rounds==1) {
    ca$round1_date = Sys.Date()-1
    ca$round2_date = Sys.Date()
    ca$round1_done_date = Sys.Date()-1
    ca$round2_done_date = NA
    ca$rounds_done = 1
  } else if (rounds==2) {
    ca$round1_date = Sys.Date()-2
    ca$round2_date = Sys.Date()-1
    ca$round1_done_date = Sys.Date()-2
    ca$round2_done_date = Sys.Date()-1
    ca$rounds_done = 2
  }
  ca = as.list(ca)[-1]
  dbUpdate(semdb, "admin",vals=ca,where=list(semester=semester))

}
