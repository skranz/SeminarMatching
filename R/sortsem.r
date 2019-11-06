# Functions for sorting the seminar list shown for students
# We want to study how the order affects choices
# Possibly we can better smooth the number of seminar participants
# by changing the order

sort.shown.seminars = function(se, seminars = se$seminars, random.prob = glob$pure.random.order.prob, glob=getApp()$glob) {
  restore.point("sort.shown.seminars")

  sem.df = seminars
  if (NROW(sem.df)<=0)
    return(sem.df)
  stud = se$stud
  seed = as.integer((stud$random_points*1e12) %% 1e9)
  seed

  res = with.random.seed(seed = seed,
    list(
      pure.random = runif(1,0,1) <= random.prob,
      view.util = runif(NROW(sem.df))
    )
  )
  pure.random = res$pure.random
  view.util = res$view.util

  if (isTRUE(stud$studBAMA=="Bachelor")) {
    sem.df$correct.bama = is.true.vec(sem.df$semBAMA %in% c("Bachelor","BA & MA"))
    #view.util[rows] = view.util[rows]+100
  } else if (isTRUE(stud$studBAMA=="Master")){
    sem.df$correct.bama = is.true.vec(sem.df$semBAMA %in% c("Master","BA & MA"))
  }

  sem.df$view.util = view.util
  if (pure.random) {
    ord = order(-sem.df$correct.bama, -sem.df$view.util)
    sem.df = sem.df[ord,]
  } else {
    sem.df = default.custom.seminar.order(se,sem.df)
  }
  write.stud.log("sort_sems",list(pure_random=pure.random, seed=seed, semorder=sem.df$semid))
  sem.df
}

default.custom.seminar.order = function(se, sem.df, app=getApp()) {
  restore.point("default.custom.seminar.order")
  db = app$glob$semdb

  round = se$round
  if (!isTRUE(round==1 | round==2)) {
    return(sem.df)
  }

  sql = '
    select semid, count(*), pos
    from studpref
    where semester = :semester AND pos <= 2 AND round = :round
    group by semid, pos;
  '
  pos.df = dbGet(db, "studpref",sql = sql, params=list(semester=se$semester,round=round))
  pos1.df = pos.df[pos.df$pos==1,1:2]
  pos2.df = pos.df[pos.df$pos==2,1:2]
  colnames(pos1.df) = c("semid","count.pos1")
  colnames(pos2.df) = c("semid","count.pos2")

  sem.df = left_join(sem.df, pos1.df, by="semid")
  sem.df = left_join(sem.df, pos2.df, by="semid")
  rows = is.na(sem.df$count.pos1)
  sem.df$count.pos1[rows] = 0
  rows = is.na(sem.df$count.pos2)
  sem.df$count.pos2[rows] = 0


  if (isTRUE(se$round==1)) {
    ord = order(
      -sem.df$correct.bama,
      -(sem.df$count.pos1 == 0),
      -(sem.df$count.pos1 == 1),
      -(sem.df$count.pos1 == 2),
      -(sem.df$count.pos1 <= 4),
      -(sem.df$slots-sem.df$count.pos1), # Number of open slots
      sem.df$count.pos2,
      -sem.df$view.util
    )
    sem.df = sem.df[ord, ]
  } else if (isTRUE(se$round==2)) {

    ord = order(
      -sem.df$correct.bama,
      -(sem.df$filled_slots + sem.df$count.pos1 == 0),
      -(sem.df$filled_slots == 0),
      -(sem.df$filled_slots + sem.df$count.pos1 == 1),
      -(sem.df$filled_slots + sem.df$count.pos1 == 2),
      -(sem.df$filled_slots + sem.df$count.pos1 == 3),
      -(sem.df$filled_slots + sem.df$count.pos1 <= 4),
      -(sem.df$slots-sem.df$filled_slots-sem.df$count.pos1), # Number of open slots
      -(sem.df$filled_slots + sem.df$count.pos2),
      -sem.df$view.util
    )
    sem.df = sem.df[ord, ]

  }

  sem.df

}

is.true.vec = function(val) {
  if (length(val)==0)
    return(val)
  val[is.na(val)] = FALSE
  return(val)
}
with.random.seed = function (expr, seed = 1234567890) {
    old.seed = get(".Random.seed", .GlobalEnv)
    set.seed(seed)
    ret = eval(expr)
    assign(".Random.seed", old.seed, .GlobalEnv)
    runif(1)
    return(ret)
}
