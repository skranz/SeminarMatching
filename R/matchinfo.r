example.seminar.match.info = function() {
  setwd("D:/libraries/SeminarMatching/semapps/shared")
  semester = "SS17"
  semid = 25

  df = seminar.match.info(semester = semester, semid=semid)

  setwd("D:/libraries/SeminarMatching/")
  write.csv(df, paste0("matchinfo_",semester,"_semid",semid,".csv"))

  setwd("D:/libraries/SeminarMatching/semapps/shared")



}


# get more detailed info about the matching for a particular seminar
seminar.match.info = function(semester, semid, seminars=NULL, students=NULL, studpref=NULL, conds=NULL, semcrit=NULL, yaml.dir=NULL, db.dir=NULL, schema.dir=NULL, matchings=NULL, semdb=NULL) {

  restore.point("seminar.match.info")

  semid_=semid
  semester_=semester

  if (is.null(yaml.dir)) yaml.dir = "./yaml"
  if (is.null(db.dir)) db.dir = "./db"
  if (is.null(schema.dir)) schema.dir = "./schema"

  if (is.null(conds))
    conds = import.semcrit.conds(yaml.dir=yaml.dir)


  if (is.null(semdb))
    semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())


  if (is.null(seminars))
    seminars = dbGet(semdb,"seminars", list(semester=semester))
  if (is.null(students))
    students = dbGet(semdb,"students", list(semester=semester))
  if (is.null(studpref))
    studpref = dbGet(semdb,"studpref", list(semester=semester, round=1))
  if (is.null(semcrit))
    semcrit = dbGet(semdb,"semcrit", list(semester=semester))

  if (is.null(matchings))
    matchings = dbGet(semdb,"matchings", list(semester=semester, round=1))


  restore.point("seminar.match.info.inner")

  semcrit = add.num.slots.to.semcrit(semcrit=semcrit, seminars=seminars)


  sem = filter(seminars, semid == semid_)
  sstuds = studpref %>%
    filter(semid==semid_) %>%
    left_join(students, by=c("userid","semester")) %>%
    arrange(pos, random_points)

  sstudpref = filter(studpref, semid==semid_)

  # Seminar criteria
  ssemcrit = semcrit %>%
    filter(semid==semid_) %>%
    filter(!is.na(points))



  ssemcrit$slot.pos = parse.semcrit.slots(ssemcrit$slots)

  # compute priorities for each student slot combination
  crit.mat = make.seminar.slots.u(sem, ssemcrit, sstuds, studpref, conds)
  crit.mat[is.na(crit.mat)] = 0

  slot.u = expand.grid(list(slot=1:sem$slots,userid=sstuds$userid),stringsAsFactors = FALSE)
  slot.u$crit.points = as.vector(crit.mat)
  min.max = slot.u %>% group_by(userid) %>% summarize(max.crit.points = max(crit.points),min.crit.points = min(crit.points))

  sstud = left_join(sstuds, min.max, by="userid")

  # join with matched seminar
  sstud = left_join(select(sstud,-semid,-round), select(matchings,userid,semid,round, slot, points, fixed_points,pos), by="userid")


  sstud = rename(sstud, pos=pos.x, pos.got=pos.y)

  sstud = mutate(sstud, member = semid == semid_)
  sstud = left_join(sstud, select(seminars,semid, semname), by="semid")
  sstud = arrange(sstud,-member, -max.crit.points, pos)

  try({
    sstud <- select(sstud, member,  pos,pos.got,points, max.crit.points, fixed_points, random_points, slot, email, studBAMA, studSubject, studSubject2, studSpecBA, studSpecMA, studSpecMA2,min.crit.points,everything())

  })
  sstud
}
