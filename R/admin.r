examples.AdminSeminarApp = function() {
  setwd("D:/libraries/SeminarMatching/testapps/")

  setwd("D:/libraries/SeminarMatching/testapps/")
  setwd("D:/libraries/SeminarMatching/semapps/")
  restore.point.options(display.restore.point = !TRUE)
  app = AdminSeminarsApp(init.userid = "sebastian.kranz@uni-ulm.de", init.password="test", lang="en", main.dir = paste0(getwd(),"/shared"))
  viewApp(app)

}

AdminSeminarsApp = function(db.dir = paste0(main.dir,"/db"), schema.dir = paste0(main.dir,"/schema"), yaml.dir =  paste0(main.dir,"/yaml"), report.dir = paste0(main.dir,"/reports"), rmd.dir =  paste0(main.dir,"/rmd"),  main.dir = getwd(),  init.userid="", init.password="", app.title="Uni Ulm WiWi Seminar Administration", app.url = "http://localhost", email.domain = "uni-ulm.de", check.email.fun=NULL, email.text.fun=default.email.text.fun, use.db=TRUE, main.header=NULL, lang="en", smtp=NULL) {
  restore.point("AdminSeminarsApp")

  library(shinyjs)
  library(loginPart)
  library(RSQLite)
  library(DBI)

  app = eventsApp()

  .GlobalEnv$knit_print.Date = function(x,...) {format(x, format="%a. %d.%m.%Y")}
  glob = app$glob

  glob$schemas = load.and.init.schemas(paste0(schema.dir, "/semdb.yaml"))
  glob$semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())

  glob$admins = dbGet(glob$semdb,"adminstaff")

  glob$report.dir = report.dir
  glob$yaml.dir = yaml.dir
  glob$schema.dir = schema.dir
  glob$db.dir = db.dir
  glob$rmd.dir = rmd.dir

  glob$sets = read.yaml(file =paste0(yaml.dir,"/sets.yaml"), utf8 = TRUE)

  form = load.and.init.form(file=paste0(yaml.dir,"/adminform.yaml"),lang=lang,prefix="adminform_")
  form$sets = glob$sets
  glob$adminform = form

  rmd.names = c("admin_overview")
  glob$rmd.li = lapply(rmd.names, function(rmd.name) {
    file = paste0(glob$rmd.dir,"/",rmd.name,"_",lang,".Rmd")
    compile.rmd(file, out.type="html",use.commonmark = TRUE)
  })
  names(glob$rmd.li) = rmd.names

 selectChangeHandler("adminform_semester", function(value, app=getApp(), se=app$se,...) {
    restore.point("semester.change.handler")
    cat("\n\nsemester changed to ", value,"\n\n")
    semester = value
    load.admin.data.from.db(semester = semester)
    show.admin.main(se=se, init.handlers=FALSE)
    #stop()
    show.admin.sems(se=se)
    show.admin.matching(se=se)
  })


  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())

  login.fun = function(app=getApp(),userid,...) {
    restore.point("login.fun")

    db = app$glob$semdb

    if (!userid %in% app$glob$admins$userid) {
      app$glob$admins = dbGet(glob$semdb,"adminstaff")
      if (!userid %in% app$glob$admins$userid) {
        show.html.warning("semAdminMainUI",paste0("The user ", userid, " has not the rights to administrate the seminar matching."))
        return()
      }
    }

    se = new.env()
    se$db = db
    se$userid = userid
    app$se = se

    ui = tabsetPanel(id = "adminTabset",
      tabPanel(title = "Overview", uiOutput("semAdminOverviewUI")),
      tabPanel(title = "Settings", uiOutput("semAdminSettingsUI")),
      tabPanel(title = "Seminars", uiOutput("semAdminSemUI")),
      tabPanel(title = "Matching", uiOutput("semAdminMatchingUI")),
      tabPanel(title = "Report",uiOutput("semAdminReportUI"))
    )
    setUI("semAdminMainUI",ui)
    changeHandler("adminTabset", change.admin.tabset)
    load.admin.data.from.db(se=se)

    show.admin.overview(se=se)
    show.admin.main(se=se)
    show.admin.sems(se=se)
    show.admin.matching(se=se)
  }

  if (is.null(check.email.fun)) {
    if (!is.null(email.domain)) {
      check.email.fun = function(email, ...) {
        check.email.domain(email,domain = email.domain)
      }
    } else {
      check.email.fun = function(email,...) {
        list(ok=TRUE,msg="")
      }
    }
  }

  lop = loginPart(db.arg = logindb.arg, login.fun=login.fun, check.email.fun=check.email.fun, email.text.fun = email.text.fun, app.url=app.url, app.title=app.title,init.userid=init.userid, init.password=init.password,container.id = "semAdminMainUI")
  set.lop(lop)
  lop.connect.db(lop=lop)
  lop$login$ui = lop.login.ui(lop)
  lop$smtp = smtp



  appInitHandler(function(session,...) {
    initLoginDispatch(lop)
  })

  app$ui = tagList(
    fluidPage(
      uiOutput("semAdminMainUI")
    )
  )
  app$lop = lop
  app
}


load.admin.data.from.db = function(semester=get.default.semester(se=se), app=getApp(), se=app$se) {
  restore.point("load.admin.data.from.db")

  if (is.null(semester))
    semester = app$glob$sets$semesters[[1]]


  admin = dbGet(se$db,"admin", nlist(semester),schema = app$glob$schemas$admin)

  if (is.null(admin)) {
    admin = empty.row.from.schema(.schema = app$glob$schemas$admin, semester=semester)
  }
  admin = as.list(admin)
  se$admin = init.se.admin(admin)
  se$update.report = TRUE
  se$report.html = NULL

  se$semester = semester

  se$seminars = dbGet(se$db,"seminars", list(semester=se$semester))
  se$studpref = dbGet(se$db,"studpref",list(semester=se$semester))
  se$students = dbGet(se$db,"students",list(semester=se$semester))

  return(invisible())
}


show.admin.main = function(userid=se$userid, yaml.dir=app$glob$yaml.dir, db=app$glob$semdb, se, semester=se$semester, app=getApp(), init.handlers=TRUE, refresh=TRUE) {

  restore.point("show.admin.main")

  se$semester = semester

  glob = app$glob
  form = glob$adminform
  form.vals = form.default.values(form,values = se$admin)
  form.ui = form.ui.simple(form, values=form.vals,add.submit = TRUE)
  clear.form.alert(form=form)


  if (init.handlers) {
    add.form.handlers(form,success.handler = save.admin.form)
    #break.point("show.admin.main.d")
    cat("\nadd selectChangeHandler")

  }

  ui = fluidRow(column(width=10, offset=1,
    form.ui
  ))

  setUI("semAdminSettingsUI", ui)

}

save.admin.form = function(values, app=getApp(), se=app$se,...) {
  restore.point("save.admin.form")

  #schema.template(values, "students")
  form = app$glob$adminform

  admin = empty.row.from.schema(app$glob$schemas$admin)
  admin = copy.intersect(admin, se$admin)
  admin = copy.intersect(admin, values)

  dbBegin(se$db)
  on.exit(try(dbRollback(se$db),silent = TRUE))
  dbDelete(se$db,"admin", admin["semester"])
  admin = dbInsert(se$db,"admin", admin)$values
  dbCommit(se$db)
  se$admin = init.se.admin(admin)

  show.form.alert(form=form,msg=form$texts$submitSuccess, color=NULL)
  show.admin.main(se=se)
}

show.admin.sems = function(se=app$se, app=getApp()) {
  restore.point("show.admin.sems")

  se$asems = filter(se$seminars,active==TRUE) %>%
    arrange(semBAMA,groupid,teacher,semname) %>%
    select(semBAMA, groupid, everything())
  df = se$asems

  if (NROW(df)>0) {
    rows = 1:NROW(df)
    if (FALSE) {
      endisBtnId = paste0("endisBtn_",rows)
      endisLabel = ifelse (df$enabled, "disable", "enable")

      endisBtns = extraSmallButtonVector(id=endisBtnId,label=endisLabel)
      cols = intersect(colnames(df), c("groupid","semname","teacher","semBAMA","area","weblink"))
      wdf = data.frame(action=endisBtns,df[,cols])
    } else {
      cols = intersect(colnames(df), c("groupid","semname","teacher","semBAMA","area","weblink"))
      wdf = data.frame(df[,cols])
    }
    html = html.table(wdf, sel.col="#aaaaaa",bg.col="#ffffff")

  } else {
    html="<p>No active seminars for this semester yet.</p>"
  }

  ui = fluidRow(column(offset=1, width=10,
    h4(paste0("Activated seminars for ", se$semester)),
    HTML(html)
  ))
  setUI("semAdminSemUI", ui)
}

show.admin.matching = function(se=app$se, app=getApp()) {
  restore.point("show.admin.matching")
  if (is.null(se$admin)) {
    setUI("semAdminMatchingUI", HTML("No matching info"))
    return()
  }
  mui = do.matching.ui(se=se)

  umui = NULL
  admin = se$admin
  if (admin$rounds_done >= admin$num_rounds) {
    us = get.unassigned(db=se$db, semester=admin$semester)
    us$studs = filter(us$studs, was_removed==0)

    if (NROW(us$studs)>0) {
      umui = tagList(
        br(),
        h4("Students who ranked at least 1 seminar but did not get a slot:"),
        HTML(html.table(us$studs))
      )
    } else {
      h4("All students who participated got a seminar slot.")
    }
  }

  ui = fluidRow(column(offset=1, width=10,
    h4(paste0("Matching for ", se$semester)),
    br(),
    mui,
    umui
  ))
  setUI("semAdminMatchingUI", ui)
  buttonHandler("doMatching1Btn", do.matching.click, round=1,se=se)
  buttonHandler("doMatching2Btn", do.matching.click, round=2,se=se)
}


init.se.admin = function(admin) {
  restore.point("init.sem.admin")
  admin = as.list(admin)
  today = as.Date(Sys.time())

  admin$after.stud.date = FALSE
  if (is.na(admin$round1_date)) {
    admin$num_rounds = 0
  } else if ((is.na(admin$round2_date))) {
    admin$num_rounds = 1
  } else {
    admin$num_rounds = 2
  }

  if (!is.na(admin$stud_start_date)) {
    admin$after.stud.date = (admin$stud_start_date <=today)
  } else {
    admin$after.stud.date = admin$rounds_done >= 0
  }

  if (admin$rounds_done == 0) {
    admin$cur_round_date = admin$round1_date
    if (admin$after.stud.date) {
      admin$selection.round = 1
    } else {
      admin$selection.round = 0
    }
  } else if (admin$rounds_done == 1) {
    if (admin$num_rounds <= 1) {
      admin$selection.round = 1
    } else {
      admin$selection.round = 2
    }
  } else if (admin$rounds_done == 2) {
    admin$selection.round = NA
  }

  if (!admin$after.stud.date) {
    admin$studmode = "pre"
  } else if (admin$rounds_done == 0) {
    admin$studmode = "round1"
  } else if (admin$rounds_done == 1 & admin$num_rounds==2) {
    admin$studmode = "round2"
  } else {
    admin$studmode = "post"
  }

  admin
}


do.matching.click = function(app=getApp(),se=app$se, round=1,...) {
  restore.point("do.matching.click")

  perform.matching(semester=se$semester,insert.into.db = TRUE, round=round)
  load.admin.data.from.db(se = se)
  show.admin.matching(se=se)
}



do.matching.ui = function(se=app$se, app=getApp()) {
  restore.point("do.matching.ui")

  admin = se$admin

  today = as.Date(Sys.time())

  ui = NULL
  if (admin$num_rounds==0) {
    ui = p("You have specified no dates for the matching, so no matching can be done.")
    return(ui)
  }

  num.pref = length(unique(se$studpref$userid))
  ui = list(p(paste0(num.pref, " students have submitted their preferences.")))

  if (num.pref == 0) {
    ui = c(ui, list(p("At least one student must have submitted their preferences for the matching do be done.")))
    return(ui)
  }

  if (admin$rounds_done == 0) {
    if (is.na(admin$round1_date)) {
      ui = c(ui, list(p("You have not yet specified a date at which the first matching round shall take place.")))
      return(ui)
    }

    if (today < admin$round1_date) {
      ui = c(ui, list(p(paste0("You have specified that the matching shall take place at ", admin$round1_date,". So you cannot run the matching already today."))))
      return(ui)
    }

    ui = c(ui, list(
      actionButton("doMatching1Btn","Perform matching"),
      helpText("Note that once you perform the matching, the results are fixed and you cannot run the matching again.")
    ))
    return(ui)
  } else if (admin$rounds_done == 1) {
    ui = c(ui, list(p("The first matching round has been performed. You can take a look at the report.")))

    if (is.na(admin$round2_date)) {
      ui = c(ui, list(p("You have not yet specified a date for a second round of matching. So there will only be a first round.")))
      return(ui)
    }

    if (today < admin$round2_date) {
      ui = c(ui, list(p(paste0("You have specified that the second matching run shall take place at ", admin$round2_date,". So you cannot run the second matching round already today the ", as.Date(Sys.time()) ))))
      return(ui)
    }

    ui = c(ui, list(
      actionButton("doMatching2Btn","Perform second matching round"),
      helpText("Note that once you perform the matching, the results are fixed and you cannot run the matching again.")
    ))
    return(ui)
  } else {
    ui = c(ui, list(p("Both matching round have been performed. You can take a look at the report.")))
  }
  return(ui)

}

change.admin.tabset = function(value,...) {
  restore.point("change.admin.tabset")

  if (value=="Report") {
    show.admin.report()
  }
  cat("Changed admin tabset...")

}

show.admin.overview = function(se=app$se, app=getApp()) {
  restore.point("show.admin.overview")
  envir = c(se$admin, list(today=as.Date(Sys.time())))

  cr = app$glob$rmd.li[["admin_overview"]]
  header = render.compiled.rmd(cr, envir=envir)
  ui = fluidRow(column(offset=1, width=10,
    HTML(header)
  ))
  setUI("semAdminOverviewUI", ui)
}

show.admin.report = function(se=app$se, app=getApp()) {
  restore.point("show.admin.report")

  report.dir = app$glob$report.dir

  if (isTRUE(se$admin$rounds_done==0)) {
    file = paste0(report.dir,"/pre_matching_admin.Rmd")
  } else {
    file = paste0(report.dir,"/matching_admin.Rmd")
  }

  if (!file.exists(file)) {
    ui = p("No report file exists.")
    setUI("semAdminReportUI",ui)
    return()
  }

  if (isTRUE(se$update.report)) {
    rmd = readLines(file,warn = FALSE)
    rmd = remove.rmd.chunks(rmd, "init_param")

    round = 1

    if (se$admin$rounds_done>0) {
      se$matchings = dbGet(se$db,"matchings",nlist(semester=se$semester))

      if (NROW(se$matchings)==0) {
        ui = p("No student has been matched.")
        setUI("semAdminReportUI",ui)
        return()
      }
    }
    # manual adjustments
    se$manual = dbGet(se$db,"manual", list(semester=se$semester))



    #writeClipboard(rmd)
    env = as.environment(list(semester=se$semester, seminars=se$seminars, students=se$students, matchings=se$matchings, studpref = se$studpref, manual=se$manual, semdb=se$db, db.dir=app$glob$db.dir, round=1))
    parent.env(env) = environment()
    rmd = paste0(rmd, collapse="\n\n")
    html = try(knit.rmd.in.temp(rmd,envir = env, fragment.only = TRUE, use.commonmark=TRUE))
    #html = try(render.rmd.in.temp(rmd,envir = env))
    if (is(html,"try-error")) {
      html = as.character(html)
    }
    se$update.report = FALSE
    se$report.html = html
    setUI("semAdminReportUI",HTML(html))
  }

}



get.default.semester = function(db = se$db, se=NULL, schemas=app$glob$schemas, app=getApp()) {
  restore.point("get.default.semester")

  today = as.Date(Sys.time())
  admins = dbGet(db,"admin",schema = schemas$admin)
  started = which(admins$default_start_date <= today)
  if (length(started)>0) {
    row = started[which.max(admins$default_start_date[started])]
    return(admins$semester[row])
  }
  return(NULL)
}

get.semesters.that.have.matchings = function(db = se$db, se=NULL) {
  sql = 'SELECT semester FROM admin WHERE round1_done_date IS NOT NULL OR round2_done_date IS NOT NULL'
  res = dbGet(db,sql = sql)
  res$semester
}
