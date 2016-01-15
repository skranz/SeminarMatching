example.random.students = function() {

}


random.students = function(n=2, semester, yaml.dir = paste0(getwd(),"/yaml")) {

  setwd("D:/libraries/SeminarMatching/semedit_app/")
  db.dir = paste0(getwd(),"/db")
  yaml.dir = paste0(getwd(),"/yaml")
  n = 50
  semester = "SS15"

  schema.file = "./schema/semdb.yaml"
  schemas = load.and.init.schemas(schema.file)
  semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())



  studform = load.and.init.form(file=paste0(yaml.dir,"/studform.yaml"), lang=lang)
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

  make.stud.pref = function(stud) {
    data.frame(
      semid = sample(seminars$semid,length(seminars$semid)),
      userid = stud$userid,
      pos = 1:NROW(seminars),
      semester = semester,
      joker = 0
    )
  }
  li = lapply(1:n, function(i) {
    make.stud.pref(studs[i,])
  })
  studpref = as_data_frame(rbindlist(li))

  dbInsert(semdb,"students", vals=studs)
  dbInsert(semdb,"studpref", vals=studpref)
}

examples.seminar.matching = function() {
  library(matchingR)
  library(YamlObjects)

  setwd("D:/libraries/SeminarMatching")
  sems = read.yaml("seminars.yaml")
  studs = read.yaml("students.yaml")
  studs


  nmen = 25
  nwomen = 20
  uM = matrix(runif(nmen*nwomen), nrow=nmen, ncol=nwomen)
  uW = matrix(runif(nwomen*nmen), nrow=nwomen, ncol=nmen)
  results = one2one(uM, uW)

  prefM = sortIndex(uM)
  prefW = sortIndex(uW)
  results = one2one(proposerPref = prefM, reviewerPref = prefW)


}

match.seminars = function(studs, sems) {
  n.stud = NROW(studs)
  studs=lapply(studs, function(stud) {
    stud$stud.id = stud$email
    stud
  })
  for (i in 1:length(studs)) {
    studs[[i]][["stud.num"]] = i
  }

  for (i in 1:length(sems)) {
    sems[[i]][["sem"]] = names(sems)[[i]]
  }

  # extract seminars
  seminars = names(sems)

  # extract background data on students and their ranking
  li = lapply(studs, function(stud) {
    cols = setdiff(names(stud),"ranking")
    stud[cols]
  })
  st.df = as.data.frame(rbindlist(li))
  st.df$base.points = runif(NROW(st.df),0,100)

  # extract students' ranking
  li = lapply(studs, function(stud) {
    rank.df = parse.student.ranking(stud$ranking, seminars, stud.id=stud$email)
  })
  st.ra = bind_rows(li)
  st.ra = inner_join(st.ra, select(st.df, stud.id, stud.num), by="stud.id")

  # create seminars' ranking
  se = sems[[1]]
  xsems = c(sems, list(none=list(sem="none",slots = n.stud)))

  li = lapply(xsems, function(se) {
    create.seminar.ranking(se = se,st.df = st.df)
  })
  se.ra = bind_rows(li)


  # expand slots

  li = lapply(xsems,function(se) {
    data_frame(sem=se$sem, slot=1:se$slots, sesl = paste0(sem,1:se$slots))
  })
  sesl.df = bind_rows(li)
  sesl.df$sesl.num = 1:NROW(sesl.df)
  xse.ra = left_join(sesl.df,se.ra, by="sem") %>% arrange(sem, slot, -points)
  xst.ra = left_join(sesl.df,st.ra, by="sem") %>% arrange(stud.id,-util,sem,slot)

  # make utility matrices
  n.stud = NROW(studs)
  n.sesl = NROW(sesl.df)

  #uM = matrix(runif(nmen*nwomen), nrow=nmen, ncol=nwomen)
  #uW = matrix(runif(nwomen*nmen), nrow=nwomen, ncol=nmen)

  # make utility matrices
  U.st = matrix(arrange(xst.ra, sesl.num,stud.num)$util, nrow=n.stud,ncol=n.sesl)
  U.sesl = matrix(arrange(xse.ra, stud.num,sesl.num)$points, nrow=n.sesl,ncol=n.stud)

  res = one2one(U.st, U.sesl)

  ma.sesl =res$proposals
  ma.sem = sesl.df$sem[ma.sesl]

  st.df$sem1 = ma.sem
}

create.seminar.ranking = function(se, st.df) {
  restore.point("create.seminar.ranking")

  ignore.fields = c("title","points","descr","weblink")
  fields = setdiff(names(se), ignore.fields)
  se.li = se[fields]

  df = cbind(st.df, as_data_frame(se.li))

  df$points = df$base.points

  p = se$points[[1]]
  cols = colnames(df)
  for (p in se$points) {
    cond = p[[2]]
    cond_ = parse.as.call(cond)
    vars = find.variables(cond_)
    unmatched = setdiff(vars,cols)
    if (length(unmatched)>0) {
      str =paste0("Points condition in seminar ",se$sem,"\n\n", cond, "\n\n contains unknown variables ",paste0(unmatched, collapse=", "), ". The condition is ignored.")
      warning(str)
      next
    }
    add.points = is.true(eval(cond_, df))
    df$points = df$points + as.numeric(p[[1]])*add.points
  }
  res = select(df,sem,slots,stud.id,stud.num, points)
  #none.df = data_frame(sem="none",slots=NROW(st.df), stud.id = st.df$stud.id, stud.num=st.df$stud.num, points=0)
  res

  #rbind(res, none.df)
}

parse.student.ranking = function(ranking, seminars, stud.id=NULL,empty.name="none") {
  restore.point("parse.student.ranking")

  add.points = length(seminars) +1

  stated.rank = sapply(ranking, function(sr) {
    if (is.null(sr)) return(NA)
    if (!is.finite(sr)) return(NA)
    return(as.numeric(sr))
  })
  stated.rank = stated.rank[!is.na(stated.rank)]


  stated = data_frame(sem=names(stated.rank),stated.rank=stated.rank)
  stated = mutate(stated,
    rank = rank(stated.rank,ties.method = "random") ,
    util = add.points- rank
  )

  #seminars = c(seminars, paste0("sem",runif(1)))
  all.sem = data_frame(sem=c(seminars,empty.name))
  df = left_join(all.sem, stated, by="sem")
  df$util[is.na(df$util)] = -1
  df$util[df$sem==empty.name] = 0
  if (!is.null(stud.id))
    df$stud.id = stud.id
  df
}

numsem = function(...) {
  0
}


