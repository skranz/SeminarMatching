
examples.perform.matching = function() {
  setwd("D:/libraries/SeminarMatching/semapps/shared")

  n = 70
  semester = "SS16"
  delete.random.students(semester=semester)
  li = draw.random.students(n=n,semester=semester,insert.into.db = TRUE)
  df = perform.matching(semester=semester,students=li$students, studpref=li$studpref,use.glob.points = 1,insert.into.db = TRUE)

  df = df %>% arrange(num_ranked, semid)

  df$has = df$semid != -1
  df %>% group_by(num_ranked) %>% summarise(matched=sum(has)/n())

  df %>% filter(has==TRUE) %>% group_by(pos) %>% summarise(count=n())


  analyse = function(use.glob.points=0) {
    df = perform.matching(semester=semester,students=li$students, studpref=li$studpref,use.glob.points = 1,insert.into.db = !TRUE, seed=seed)

    df$has = df$semid != -1

    mean.share = as.numeric(df %>% filter(num_ranked==3) %>% summarise(matched=sum(has)/n()))
    mean.pos = mean(df$pos)

    list(use.glob.points=use.glob.points,mean.share3=mean.share, mean.rank=mean.pos, rank1=sum(df$pos==1)/NROW(df))

  }
  analyse(0.1, seed=seed)
  seed = as.integer(Sys.time())
  li = draw.random.students(n=n,semester=semester,insert.into.db = !TRUE)

  res.li = lapply(seq(0,1,by=0.2),analyse,seed=seed)
  res = rbindlist(res.li)
  res
}

perform.matching = function(round=1,semester=se[["semester"]],seminars=NULL,students=NULL, studpref=NULL, semcrit=NULL, semdb=se[["db"]], conds=glob[["conds"]], sets=glob[["sets"]], schemas=glob$schemas, se=NULL, dirs=glob, glob=getApp()$glob, yaml.dir=dirs$yaml.dir, db.dir=dirs$db.dir, schema.dir=dirs$schema.dir, reload=FALSE, seed=NULL, insert.into.db=TRUE, use.glob.points = TRUE) {
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
    studpref = dbGet(semdb,"studpref", list(semester=semester, round=round))
  if (is.null(semcrit))
    semcrit = dbGet(semdb,"semcrit", list(semester=semester))

  semcrit = filter(semcrit, !is.na(points))

  semcrit$slot.pos = parse.semcrit.slots(semcrit$slots)



  num.studs = NROW(students)
  num.sems = NROW(seminars)
  num.slots = sapply(1:num.sems,function(row) seminars$slots[row])
  total.slots = sum(num.slots)

  # Fixed seminar slot utilities over students coming from priorities
  seu.li = lapply(1:num.sems, function(row) {
    fixed = make.seminar.slots.u(seminars[row,], semcrit, students, studpref, conds)
    fixed
  })

  # matrix with total.slots rows and num.studs columns
  fixed.seu = do.call(rbind,seu.li)

  if (is.null(seed)) seed = as.integer(Sys.time())
  set.seed(seed)
  students$glob.points = runif(NROW(students),0,10)

  # Random basic points for each seminar
  # Students either have a global random value
  # or we draw the value separately for each seminar
  # Global values are more likely to be pareto-efficient
  # but separate values benefit students who rank many
  # seminars compared to those that rank a few.
  # Assuming that students who rank many seminars
  # have higher utility from getting a seminar slot,
  # this may be beneficial.


  sem.base.points =
    use.glob.points*matrix(students$glob.points,num.sems,num.studs,byrow=TRUE) +
    (1-use.glob.points)*matrix(runif(num.sems*num.studs,0,10),num.sems,num.studs)

  sem.of.slot = unlist(lapply(1:num.sems, function(sem.pos) rep(sem.pos,seminars$slots[sem.pos])))
  slot.of.slot = unlist(lapply(1:num.sems, function(sem.pos) if (num.slots[sem.pos]>0) 1:num.slots[sem.pos] else NULL))

  # Expand the random base points to slots
  random.seu = sem.base.points[sem.of.slot,]

  # Seminar slots utility over students
  seu = fixed.seu + random.seu

  # empty seminar, which will be assigned to students who do not
  # get a seminar
  empty.sem = matrix(runif(num.studs*num.studs),nrow=num.studs)

  # add empty seminar
  seu = rbind(seu, empty.sem)
  random.seu = rbind(random.seu, empty.sem)
  fixed.seu = seu-random.seu



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

  # The matched seminars for each student are in
  # gs$proposals

  # Now we will store the matchings in a data frame that also
  # contains info about the seminar pos in the student's ranking
  # and the point the student got
  sem.of.slot = c(sem.of.slot, rep(num.sems+1, num.studs))
  slot.of.slot = c(slot.of.slot, 1:num.studs)

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
    fixed_points = fixed.seu[cbind(gs$proposals,1:num.studs)],
    random_points = random.seu[cbind(gs$proposals,1:num.studs)]
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

  df$round = round
  df$semester = semester
  df$joker = 0
  df = arrange(df, semid, slot)

  if (insert.into.db) {
    restore.point("perform.matchin.db")

    if (is.null(schemas))
      schemas = load.and.init.schemas(paste0(schema.dir,"/semdb.yaml"))

    dbBegin(semdb)
    res = try({
      if (round==1) {
        # delete also later matching rounds
        dbDelete(semdb,"matchings",nlist(semester))
        dbDelete(semdb,"assign",nlist(semester, assign_method="r1"))
        dbDelete(semdb,"assign",nlist(semester, assign_method="r2"))

        dbUpdate(semdb,"admin",list(rounds_done=round, round1_seed=seed, round1_done_date=as.Date(Sys.time())), where=nlist(semester), schema = schemas$admin)
      } else if (round==2) {
        dbDelete(semdb,"matchings",nlist(semester,round))
        dbDelete(semdb,"assign",nlist(semester, assign_method="r2"))
        dbUpdate(semdb,"admin",list(rounds_done=round, round2_seed=seed, round2_done_date=as.Date(Sys.time())), where=nlist(semester),schema = schemas$admin)
      } else {
        stop("round must be 1 or 2")
      }

      # Store in matching
      dbInsert(semdb,"matchings",df,schema = schemas$matchings)
      # Also store into assign
      adf = select(df,semid,userid,semester) %>%
            filter(semid != -1) %>%
            mutate(assign_method = paste0("r",round),topic_ind=NA_integer_,assign_time=Sys.time())
      dbInsert(semdb,"assign",adf,schema = schemas$assign)
    })
    if (is(res,"try-error")) {
      dbRollback(semdb)
    } else {
      dbCommit(semdb)
    }

    update.seminar.filled_slots(semid=NULL, semester=semester,semdb=semdb)
  }

  invisible(df)
  #invisible(nlist(df, seu, stu, studpref, sem.of.slot, slot.of.slot, fixed.seu, random.seu, base.points=base.points, seed=seed)
}

# Update number of filled slots in seminar table
update.seminar.filled_slots = function(semid=se$semid, semester=se$semester, filled_slots=NULL,semdb=se$semdb,se=NULL) {
  restore.point("update.seminar.filled.slots")
  if (is.null(filled_slots)) {
    # Update filled_slots
    if (is.null(semid)) {
      assign = dbGet(semdb,"assign",nlist(semester=semester))
      semid = unique(assign$semid)
    } else {
      assign = dbGet(semdb,"assign",nlist(semester=semester, semid=semid))
    }
    df = assign %>% group_by(semid) %>% summarise(filled_slots=n())
    filled_slots = df$filled_slots[match(semid,df$semid)]
  }
  dbBegin(semdb)
  tryCatch({
    for (i in seq_along(semid)) {
      dbUpdate(semdb,"seminars", vals=list(filled_slots=filled_slots[i]), where=nlist(semester, semid=semid[i]))
    }
  }, error=function(...) {dbRollback(semdb)})
  dbCommit(semdb)
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

make.seminar.slots.u = function(sem, semcrit, students, studpref, conds, base.points = rep(0, NROW(students))) {
  restore.point("make.seminar.slots.u")


  num.slots = sem$slots
  num.studs = NROW(students)
  if (num.slots==0) return(NULL)

  slostu = expand.grid(stud=students$userid,slot=1:sem$slots)

  points = base.points

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
    #cat(co$condition)
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
  semester.add = paste0(" and semester in ", dplyr::escape(semester, parens=TRUE))

  sql = paste0("delete from matchings where userid in ", escaped)
  if (!is.null(semester)) sql = paste0(sql,semester.add)
  dbSendQuery(db, sql)


  sql = paste0("delete from studpref where userid in ", escaped)
  if (!is.null(semester)) sql = paste0(sql,semester.add)
  dbSendQuery(db, sql)

  sql = paste0("delete from students where userid in ", escaped)
  if (!is.null(semester)) sql = paste0(sql,semester.add)
  dbSendQuery(db, sql)

}

draw.random.students = function(n=2, semester, round=1, insert.into.db=FALSE, yaml.dir = "./yaml", db.dir = "./db", schema.dir = "./schema", counter.name=TRUE) {
  restore.point("draw.random.students")

  semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())

  schema.file = paste0(schema.dir,"/semdb.yaml")
  schemas = load.and.init.schemas(schema.file)

  studform = load.and.init.form(file=paste0(yaml.dir,"/studform.yaml"), lang="en",warn.no.prefix = FALSE)
  sets = read.yaml(file =paste0(yaml.dir,"/sets.yaml"), utf8 = TRUE)

  empty.stud  = empty.row.from.schema(schemas$students, semester=semester)

  random.stud = function(i,...) {
    restore.point("random.stud")
    stud = form.random.values(studform,sets = sets)
    stud$semester = semester
    if (!counter.name) {
      name = paste0("random__", sample.int(.Machine$integer.max,1))
    } else {
      name = paste0("random__",i)
    }
    stud$userid = stud$email = stud$name = name
    stud = stud[names(empty.stud)]

    stud
  }

  li = lapply(1:n, random.stud)
  studs = as_data_frame(rbindlist(li))

  seminars = dbGet(semdb,"seminars", list(semester=semester))

  # Create some weights to make some seminars more attractive
  sem.weights = runif(NROW(seminars))
  sem.weights = sem.weights / sum(sem.weights)
  make.stud.pref = function(stud,num.pos = sample.int(NROW(seminars),1)) {
    data_frame(
      semid = sample(seminars$semid,num.pos,prob=sem.weights),
      userid = stud$userid,
      pos = 1:num.pos,
      semester = semester,
      round=round,
      joker = 0
    )
  }
  li = lapply(1:n, function(i) {
    make.stud.pref(studs[i,], num.pos = ((i-1) %% NROW(seminars))+1)
  })
  studpref = as_data_frame(rbindlist(li))

  if (insert.into.db) {
    dbInsert(semdb,"students", vals=studs)
    dbInsert(semdb,"studpref", vals=studpref)
  }

  invisible(list(students = studs, studpref=studpref))
}


