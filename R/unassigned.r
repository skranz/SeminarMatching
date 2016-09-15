

fetch.unassigned.students = function(db, semester, app=getApp()) {
  restore.point("fetch.unassigned.students")
  sql =
'
select a.*, b.semid, b.pos, b.round, c.semname from students AS a, studpref AS b, seminars AS c
where a.semester = :semester
and b.userid = a.userid
and b.semester = a.semester
and b.semid = c.semid
and a.userid not in (select userid from assign where semester = semester)
'
  stupref = dbGet(db, sql=sql, params=nlist(semester))

  d = stupref %>% group_by(semester,email) %>%
    arrange(email,round, pos) %>%
    mutate(random_points=round(random_points,3),num_sem_ranked = length(unique(semid)), ranked_seminars=paste0(unique(semname), collapse=", ")) %>%
    select(-semname, -pos,-userid, -name, -semid, -round, -semester)

  d = d[!duplicated(d$email),] %>%
    arrange(-num_sem_ranked) %>%
    select(num_sem_ranked, email, random_points, everything())

  app$glob$unassigned.students = list(time=Sys.time(),semester=semester, prefs=stupref, studs=d)
  invisible(app$glob$unassigned.students)
}

get.unassigned = function(semester, app=getApp(), update.max = 60*30, update=FALSE, db) {
  restore.point("get.unassigned")

  us = app$glob$unassigned.students
  if (is.null(us) | update)
    return(fetch.unassigned.students(db=db, semester=semester))
  if (as.numeric(Sys.time() - us$time) > update.max | us$semester != semester) {
    return(fetch.unassigned.students(db=db, semester=semester))
  }
  us
}

