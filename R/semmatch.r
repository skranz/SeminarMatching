
examples.perform.matching = function() {
  setwd("D:/libraries/SeminarMatching/semedit_app/")

  n = 100
  semester = "SS15"
  delete.random.students(semester=semester)

  li = draw.random.students(n=n,semester=semester,insert.into.db = !TRUE)
  df = perform.matching(semester=semester,students=li$students, studpref=li$studpref )


}

perform.matching = function(round=1,semester=se[["semester"]],seminars=NULL,students=NULL, studpref=NULL, semcrit=NULL, semdb=se[["db"]], conds=glob[["conds"]], sets=glob[["sets"]], se=NULL, glob=getApp()$glob, yaml.dir=glob$yaml.dir, db.dir=glob$db.dir, schema.dir=glob$schema.dir, reload=FALSE) {
  restore.point("perform.matching")


  if (is.null(yaml.dir)) yaml.dir = "./yaml"
  if (is.null(db.dir)) db.dir = "./db"
  if (is.null(schema.dir)) schema.dir = "./schema"

  if (is.null(semdb))
    semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())

  set = glob$sets

  if (is.null(sets))
    sets = read.yaml(file =paste0(yaml.dir,"/sets.yaml"), utf8 = TRUE)
  if (is.null(conds))
    conds = import.semcrit.conds(yaml.dir=yaml.dir)

  if (reload) {
    seminars=students=studpref=semcrit=NULL
  }

  if (is.null(seminars))
    seminars = dbGet(semdb,"seminars", list(semester=semester))
  if (is.null(students))
    students = dbGet(semdb,"students", list(semester=semester))
  if (is.null(studpref))
    studpref = dbGet(semdb,"studpref", list(semester=semester))
  if (is.null(semcrit))
    semcrit = dbGet(semdb,"semcrit", list(semester=semester))

  semcrit = filter(semcrit, !is.na(points))

  semcrit$slot.pos = parse.semcrit.slots(semcrit$slots)

  students$glob.points = runif(NROW(students),0,10)

  num.studs = NROW(students)
  num.sems = NROW(seminars)
  num.slots = sapply(1:num.sems,function(row) seminars$slots[row])
  total.slots = sum(num.slots)

  # Seminar slot utilities over students
  seu.li = lapply(1:num.sems, function(row) {
    make.seminar.slots.u(seminars[row,], semcrit, students, studpref, conds)
  })

  # matrix with total.slots rows and num.studs columns
  seu = do.call(rbind,seu.li)

  # empty seminar, which will be assigned to students who do not
  # get a seminar
  empty.sem = matrix(runif(num.studs*num.studs),nrow=num.studs)

  # add empty seminar
  seu = rbind(seu, empty.sem)

  # create student utility over seminars
  studpref$pos.points = (num.sems+1 - studpref$pos)
  studpref$stud.pos = match(studpref$userid, students$userid)
  studpref$sem.pos = match(studpref$semid, seminars$semid)

  # students utility for each seminar
  # num.stud rows and num.sems cols
  # not ranked seminars get a negative number
  sstu = matrix(-1,nrow=num.studs, ncol=NROW(seminars))
  sstu[cbind(studpref$stud.pos,studpref$sem.pos)] = studpref$pos.points


  # transform to matrix with
  # num.stud rows and total.slots cols
  sem.of.slot = unlist(lapply(1:num.sems, function(sem.pos) rep(sem.pos,seminars$slots[sem.pos])))
  stu = sstu[,sem.of.slot]
  # add zero utility for empty seminar slots
  stu = cbind(stu, matrix(0,nrow=num.studs,ncol=num.studs))

  # add small number to break ties between slots
  # earlier slots are preferred by all students
  tie.break = ( (NCOL(stu)-1):0) / (10^(ceiling(log(NCOL(stu)+1,10))))
  stu = stu + matrix(tie.break,ncol=NCOL(stu),nrow=NROW(stu),byrow = TRUE)


  # we can now perform the matching with
  # stu (students' utility over seminar slots) and
  # seu (seminar slots utility over students)


  gs = galeShapley.marriageMarket(proposerUtils = t(stu),reviewerUtils = t(seu))

  # The macthed seminars for each student are in
  # gs$proposals

  # Now we will store the matchings in a data frame that also
  # contains info about the seminar pos in the student's ranking
  # and the point the student got
  sem.of.slot = c(unlist(lapply(1:num.sems, function(sem.pos) rep(sem.pos,seminars$slots[sem.pos]))), rep(num.sems+1, num.studs))
  slot.of.slot = c(unlist(lapply(1:num.sems, function(sem.pos) if (num.slots[sem.pos]>0) 1:num.slots[sem.pos] else NULL)), 1:num.studs)

  semids  = c(seminars$semid, -1)
  sempos  = sem.of.slot[gs$proposals]
  gs.semid = semids[sempos]
  gs.slot = slot.of.slot[gs$proposals]

  # the resulting matching as a data frame
  df = data_frame(
    userid = students$userid,
    semid  = gs.semid,
    slot = gs.slot,
    points = seu[cbind(gs$proposals,1:num.studs)],
    glob_points = students$glob.points
  )

  # check the number of matched students for each seminar
  group_by(df,semid) %>% summarise(num.studs = n()) %>% arrange(-num.studs)

  # find the pos of the seminar in the student's ranking
  df = left_join(df,select(studpref,semid,userid,pos),by=c("userid","semid"))

  # store the number of ranked seminars
  mr = group_by(studpref,userid) %>% summarise(num_ranked = max(pos))
  df = left_join(df,mr,by=c("userid"))

  # set the rank for non-matched students to num_ranked+1
  rows = is.na(df$pos)
  df$pos[rows] = df$num_ranked[rows]+1

  df = arrange(df, semid, slot)

  df
}

students.satisfy.semcrit = function(sc, students, studpref, conds) {
  restore.point("students.satisfy.semcrit")

  cfields = intersect(names(conds), names(sc))
  cfields = cfields[nchar(sc[cfields])>0]

  #field = cfields[[1]]
  ok = rep(TRUE, NROW(students))
  for (field in cfields) {
    cond = conds[[field]]
    subst = nlist(sc[[field]])
    names(subst) = paste0(".", field)
    cond = substitute.call(cond, subst)
    cond.ok = eval(cond, students)
    ok = ok & cond.ok
  }
  ok
}

parse.semcrit.slots = function(slots) {
  has.slots = nchar(slots)>0

  sv = vector("list", length(slots))
  pslots = lapply(slots[has.slots],function(str) {
    chars = strsplit(str,"")[[1]]
    allowed = c(as.character(0:9),","," ",":")
    if (any(!(chars %in% allowed))) {
      stop(paste0("Invalid slot range: ", str),call. = FALSE)
    }

    grepl(pattern = '[a-zA-Z$]',str)
    code = paste0("c(",str,")")
    res = try(eval(parse(text=code)))
    if (is(res,"try-error")) {
      stop(paste0("Invalid slot range: ", str),call. = FALSE)
    }
    res
  })
  sv[has.slots] = pslots
  sv
}

make.seminar.slots.u = function(sem, semcrit, students, studpref, conds) {
  restore.point("make.seminar.slots.u")


  num.slots = sem$slots
  num.studs = NROW(students)
  if (num.slots==0) return(NULL)

  slostu = expand.grid(stud=students$userid,slot=1:sem$slots)

  points = students$glob.points

  scs = filter(semcrit, semid==sem$semid)

  scs.ind = 1
  if (NROW(scs)>0) {
    for (scs.ind in 1:NROW(scs)) {
      # Which students satisfy criterion
      sc = scs[scs.ind,]
      stud.ok = students.satisfy.semcrit(sc,students=students, studpref=studpref, conds=conds)
      if (length(sc$slot.pos)>0) {
        slot.ok = (1:num.slots) %in% sc$slot.pos[[1]]
      } else {
        slot.ok = rep(TRUE, num.slots)
      }
      stud.ok = rep(stud.ok, times =  num.slots)
      slot.ok = rep(slot.ok, each = num.studs)
      points = points + (stud.ok & slot.ok)*sc$points
    }
  }

  mat = t(matrix(points,nrow=num.studs,ncol=num.slots))
  mat
}



import.semcrit.conds = function(file =paste0(yaml.dir,"/conditions.yaml"), yaml.dir =  "./yaml") {
  restore.point("import.semcrit.conds")

  fconds = read.yaml(file, utf8 = TRUE)
  conds = lapply(fconds$fields, function(co) {
    restore.point("njnfngh")
    cat(co$condition)
    parse.as.call(text=co$condition)
  })
  conds
}

example.draw.random.students = function() {
  setwd("D:/libraries/SeminarMatching/semedit_app/")
  db.dir = paste0(getwd(),"/db")
  yaml.dir = paste0(getwd(),"/yaml")
  n = 30
  semester = "SS15"

  delete.random.students(db.dir=db.dir, semester=semester)

  res = draw.random.students(n=n,semester=semester,insert.into.db = TRUE)
}

delete.random.students = function(semester=NULL, db = NULL, db.dir = "./db") {
  restore.point("delete.random.students")

  if (is.null(db)) {
    db = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())
  }
  par = NULL
  if (!is.null(semester)) par = list(semester=semester)

  studs = dbGet(db, "students",par)
  rows = str.starts.with(studs$userid,"random__") & str.starts.with(studs$name,"random__")
  studs = studs[rows,]

  # userid to be deleted
  userids = studs$userid

  if (length(userids)==0) {
    cat("No random users found")
    return()
  }
  # to prevent sql injection, properly escape the strings
  # this already pastes them together
  escaped = dplyr::escape(userids, parens=TRUE)


  sql = paste0("delete from studpref where userid in ", escaped)
  if (!is.null(semester)) sql = paste0(sql, " and semester in ", dplyr::escape(semester, parens=TRUE))
  if (length(sql)>1) stop("dplyr::escape suddenly returns a vector!")
  dbSendQuery(db, sql)

  sql = paste0("delete from students where userid in ", escaped)
  if (!is.null(semester)) sql = paste0(sql, " and semester in ", dplyr::escape(semester,parens=TRUE))
  dbSendQuery(db, sql)

}

draw.random.students = function(n=2, semester, insert.into.db=FALSE, yaml.dir = "./yaml", db.dir = "./db", schema.dir = "./schema") {
  restore.point("draw.random.students")

  semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())

  schema.file = paste0(schema.dir,"/semdb.yaml")
  schemas = load.and.init.schemas(schema.file)

  studform = load.and.init.form(file=paste0(yaml.dir,"/studform.yaml"), lang="en")
  sets = read.yaml(file =paste0(yaml.dir,"/sets.yaml"), utf8 = TRUE)

  empty.stud  = empty.row.from.schema(schemas$students, semester=semester)

  random.stud = function(...) {
    restore.point("random.stud")
    stud = form.random.values(studform,sets = sets)
    stud$semester = semester
    stud$userid = stud$email = stud$name = paste0("random__", sample.int(.Machine$integer.max,1))
    stud = stud[names(empty.stud)]

    stud
  }

  li = lapply(1:n, random.stud)
  studs = as_data_frame(rbindlist(li))

  seminars = dbGet(semdb,"seminars", list(semester=semester))

  make.stud.pref = function(stud,num.pos = sample.int(NROW(seminars),1)) {
    data_frame(
      semid = sample(seminars$semid,num.pos),
      userid = stud$userid,
      pos = 1:num.pos,
      semester = semester,
      joker = 0
    )
  }
  li = lapply(1:n, function(i) {
    make.stud.pref(studs[i,])
  })
  studpref = as_data_frame(rbindlist(li))

  if (insert.into.db) {
    dbInsert(semdb,"students", vals=studs)
    dbInsert(semdb,"studpref", vals=studpref)
  }

  invisible(list(students = studs, studpref=studpref))
}


