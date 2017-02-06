

fetch.unassigned.students = function(db, semester, app=getApp(), no.2nd=TRUE) {
  restore.point("fetch.unassigned.students")
  # old code
  if (FALSE) {
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
  }

  sql =
'
select a.*, b.semid, b.pos, b.round, c.semname from students AS a, studpref AS b, seminars AS c
where a.semester = :semester
and b.userid = a.userid
and b.semester = a.semester
and b.semid = c.semid
'
  astupref = dbGet(db, sql=sql, params=nlist(semester))
  assign = dbGet(db,"assign",nlist(semester))
  stupref = filter(astupref, !userid %in% assign$userid)
  stupref$got_sems = rep(0,NROW(stupref))

  # add students that got a seminar in round 1
  # but want a second seminar
  if (no.2nd) {
    r2.stupref = filter(astupref, round==2)
    r2.userid = unique(r2.stupref$userid)
    r1.userid = unique(filter(assign, assign_method=="r1")$userid)
    beyond.r1.userid = unique(filter(assign, assign_method!="r1")$userid)

    extra.users = setdiff(intersect(r2.userid, r1.userid),beyond.r1.userid)
    extra.stupref = filter(astupref, round==2, userid %in% extra.users)
    if (NROW(extra.stupref)>0) {
      extra.stupref$got_sems = 1
      stupref = rbind(stupref, extra.stupref)
    }
  }



  d = stupref %>% group_by(semester,email) %>%
    arrange(email,pos, round) %>%
    mutate(random_points=round(random_points,3),num_sem_ranked = length(unique(semid)), ranked_seminars=paste0(unique(semname), collapse=", ")) %>%
    ungroup %>%
    select(-semname, -pos,-userid, -name, -semid, -semester, -round)


  d = d[!duplicated(d[,c("email")]),] %>%
    arrange(got_sems,-num_sem_ranked) %>%
    select(got_sems,num_sem_ranked, email, random_points, everything())

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

