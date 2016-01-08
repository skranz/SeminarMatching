examples.AdminSeminarApp = function() {
  setwd("D:/libraries/SeminarMatching/semedit_app/")
  db.dir = paste0(getwd(),"/db")

  restore.point.options(display.restore.point = TRUE)

  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())

  app = AdminSeminarsApp(db.dir = db.dir, init.userid = "test", init.password="test", lang="de")

  runEventsApp(app, launch.browser = rstudio::viewer)

}

AdminSeminarsApp = function(db.dir = paste0(getwd(),"/db"), schema.dir = paste0(getwd(),"/schema"), yaml.dir =  paste0(getwd(),"/yaml"),   init.userid="", init.password="", app.title="Uni Ulm WiWi Seminar Administration", app.url = "http://localhost", email.domain = "uni-ulm.de", check.email.fun=NULL, email.text.fun=default.email.text.fun, use.db=TRUE, main.header=NULL, lang="en") {
  restore.point("EditSeminarsApp")

  library(shinyjs)
  library(loginPart)
  library(RSQLite)
  library(DBI)

  app = eventsApp()

  glob = app$glob

  glob$schemas = load.and.init.schemas(paste0(schema.dir, "/semdb.yaml"))
  glob$semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())

  glob$admins = dbGet(glob$semdb,"adminstaff")


  glob$yaml.dir = yaml.dir
  glob$schema.dir = schema.dir
  glob$db.dir = db.dir

  glob$sets = read.yaml(file =paste0(yaml.dir,"/sets.yaml"), utf8 = TRUE)

  form = load.and.init.form(file=paste0(yaml.dir,"/adminform.yaml"),lang=lang)
  form$sets = glob$sets
  glob$adminform = form

  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())

  login.fun = function(app=getApp(),userid,...) {
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
    show.admin.main(se=se)
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
  lop$smtp = lop.get.smtp()

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


load.admin.data.from.db = function(semester=se$semester, app=getApp(), se=app$se) {
  restore.point("load.admin.data.from.db")

  if (is.null(semester)) {
    semester = dbGet(se$db,"settings")$semester
    if (length(semester)==0) {
      semester = app$glob$sets$semesters[[1]]
    } else if (length(semester)>1) {
      semester = semester[[1]]
    }
  }
  se$semester = semester

  se$seminars = dbGet(se$db,"seminars", list(semester=se$semester))

  se$ses = dbGet(se$db,"semsettings",list(semester=se$semester),
    schema=app$glob$schemas$semsettings)

  if (is.null(se$ses)) se$ses = list(semester=se$semester)

  return(invisible())
}


show.admin.main = function(userid=se$userid, yaml.dir=app$glob$yaml.dir, db=app$glob$semdb, se=NULL, app=getApp(), refresh=TRUE) {
  semester = "SS15"
  restore.point("show.edit.sem.main")

  if (refresh)
    load.admin.data.from.db(se=se)

  glob = app$glob
  form = glob$adminform
  form.vals = form.default.values(glob$studform,values = se$stud)
  form.ui = form.ui.simple(glob$studform, values=form.vals,add.submit = TRUE)
  clear.form.alert(form=form)

  add.form.handlers(form,success.handler = save.studform)


  setUI("studformUI", form.ui)

}

save.studform = function(values, app=getApp(), se=app$se,...) {
  restore.point("save.studForm")

  #schema.template(values, "students")
  form = app$glob$studform

  se$stud[names(values)] = values
  se$stud$semester = se$semester
  se$stud.exists = TRUE
  se$stud$userid = se$userid

  dbBegin(se$db)
  dbDelete(se$db,"students", se$stud[c("userid","semester")])
  dbInsert(se$db,"students", se$stud)
  dbCommit(se$db)

  show.form.alert(form=form,msg=form$texts$submitSuccess, color=NULL)
  show.stud.sem.ui(se=se)
}

show.stud.sem.ui = function(se=app$se, app=getApp(), refresh.se = FALSE) {
  restore.point("show.stud.sem.ui")

  if (refresh.se)
    se = refresh.stud.app.data(se=se)

  if (!isTRUE(se$stud.exists)) {
    ui = fluidRow(column(offset=1, width=10,
      HTML(app$glob$opts$noSemPrefMsg[[app$lang]])
    ))
    setUI("studsemUI", ui)
    return()
  }

  lang = app$lang
  glob = app$glob
  opts = glob$opts
  sems = se$sems

  cols = c("semid",intersect(union(opts$selSemCols,opts$allSemCols),colnames(sems)))
  sem.df = sems[,cols]

  # Add links to seminar titles
  if ("weblink" %in% colnames(sems)) {
    rows = which(nchar(sems$weblink)>0)
    sem.df$name[rows] = paste0('<a href="', sems$weblink[rows],'" target = "_blank">',sem.df$name[rows],'</a>')
  }

  sem.df$selected = FALSE
  sem.df$row = 1:NROW(sem.df)
  sem.df$pos = NA
  sem.df$joker = 0

  #app$ui = fluidPage(dataTableOutput("selTable"))



  if (NROW(se$studpref)>0) {
    sel.rows = match(se$studpref$semid, sem.df$semid)
    sel.df = sem.df[sel.rows,]
    sel.df$pos = 1:NROW(sel.df)
    sel.df$joker = se$studpref$joker
  } else {
    sel.rows = integer(0)
    sel.df = sem.df[sel.rows,]
  }
  update.selTable(sel.df)
  update.semTable(sem.df, sel.rows=sel.rows)


  ui = fluidRow(column(offset=1, width=10,
    HTML(opts$rankingTitle[[lang]]),
    p(opts$rankingDescr[[lang]]),
    bsCollapse(bsCollapsePanel(
      title="Details",
      HTML(opts$rankingBackground[[lang]])
    )),
    h3(opts$selSemTitle[[lang]]),
    uiOutput("selSemUI"),
    br(),
    actionButton("saveStudprefBtn",opts$rankingSaveBtnLabel[[lang]]),
    bsAlert("saveStudprefAlert"),
    h3(opts$allSemTitle[[lang]]),
    uiOutput("allSemUI")
  ))

  buttonHandler("saveStudprefBtn",save.studpref)
  add.studpref.handlers(num.sems=NROW(sem.df))
  setUI("studsemUI", ui)

}


update.selTable = function(sel.df, sel.row=NULL, app=getApp(), se=app$se) {
  restore.point("update.selTable")

  se$sel.df = sel.df
  if (NROW(sel.df)==0) {
    setUI("selSemUI",p("---"))
    return()
  }
#  cat("\nsel.df: \n\n")
#  print(sel.df)

  widget.df = sel.widgets.df(sel.df)
  html = hwrite.selTable(widget.df,sel.row=sel.row)
#  cat("selTable html: \n\n", html)
  setUI("selSemUI",HTML(html))

}


hwrite.selTable = function(widget.df, sel.row=1, app=getApp(), opts=app$opts, lang=app$lang) {
  restore.point("hwrite.selTable")
  header = opts$selSemColnames[[lang]]
  if (!app$glob$use_joker) {
    header = setdiff(header, c("joker","Joker"))
  }

  html.table(widget.df,sel.row = sel.row, header=header, bg.color="#ffffff")
}

sel.widgets.df = function(df, cols=app$opts$selSemCols, app=getApp()) {
  restore.point("sel.widgets.df")

  rows = 1:NROW(df)
  upBtnId = paste0("upBtn_",rows)
  downBtnId = paste0("downBtn_",rows)
  removeBtnId = paste0("removeBtn_",rows)
  jokerBtnId = paste0("jokerBtn_",rows)

  upBtns = extraSmallButtonVector(id=upBtnId,label="",icon=icon("arrow-up",lib = "glyphicon"))
  downBtns = extraSmallButtonVector(id=downBtnId, label="",icon=icon("arrow-down",lib="glyphicon"))
  removeBtns = extraSmallButtonVector(id=removeBtnId, label="",icon=icon("remove",lib = "glyphicon"))

  if (app$glob$use_joker) {
    jokerBtns = extraSmallButtonVector(id=jokerBtnId, label="",icon=icon("star-empty",lib = "glyphicon"))

    srows = which(df$joker>0)
    if (length(srows)>0) {
      jokerBtns[srows] = extraSmallButtonVector(id=jokerBtnId[srows], label="",icon=icon("star",lib = "glyphicon"))
    }

  }


  btns = paste0(upBtns,downBtns,removeBtns)
  if (app$glob$use_joker) {
    data.frame(Rank=rows,Joker =jokerBtns, btns,df[,cols])
  } else {
    data.frame(Rank=rows, btns,df[,cols])
  }
}


update.semTable = function(sem.df, sel.rows=which(sem.df$selected), app=getApp(), se=app$se) {
  restore.point("update.semTable")

  sem.df$selected[sel.rows] = TRUE

  se$sem.df = sem.df

  widget.df = sem.widgets.df(sem.df)
  html = hwrite.semTable(widget.df,sel.rows=sel.rows)
  setUI("allSemUI",HTML(html))


}

hwrite.semTable = function(widget.df, sel.rows=NULL,app=getApp(), lang=app$lang, opts=app$opts) {
  restore.point("hwrite.semTable")
  html.table(widget.df,sel.row = sel.rows, header=opts$allSemColnames[[lang]], bg.color="#ffffff", sel.color="#aaffaa")
}


sem.widgets.df = function(df, cols=app$opts$selSemCols, app=getApp()) {
  restore.point("sem.widgets.df")
  rows = 1:NROW(df)
  addBtnId = paste0("addBtn_",rows)
  addBtns = extraSmallButtonVector(id=addBtnId,label="",icon=icon("plus",lib = "glyphicon"))
  addBtns[df$selected] = ""
  btns = paste0(addBtns)
  data.frame(row=rows,buttons=btns,df[,cols])
}



add.studpref.handlers = function(num.sems) {
  rows = 1:num.sems

  upBtnId = paste0("upBtn_",rows)
  downBtnId = paste0("downBtn_",rows)
  removeBtnId = paste0("removeBtn_",rows)
  addBtnId = paste0("addBtn_",rows)
  jokerBtnId =  paste0("jokerBtn_",rows)


  for (row in rows) {
    buttonHandler(upBtnId[row],updown.click, row=row,up=TRUE)
    buttonHandler(downBtnId[row],updown.click, row=row,up=FALSE)
    buttonHandler(addBtnId[row],add.seminar.click, row=row)
    buttonHandler(removeBtnId[row],remove.seminar.click, pos=row)
    buttonHandler(jokerBtnId[row],joker.seminar.click, pos=row)
  }

}


updown.click = function(app=getApp(),value,row,up=TRUE,se=app$se,...) {
  sel.df = se$sel.df
  restore.point("updown.click")
  cat("updown.click")

  new.pos = row + 1.5 - 3*up
  sel.df$pos[row] = new.pos
  sel.df = sel.df[order(sel.df$pos),]
  new.row = which(sel.df$pos==new.pos)
  sel.df$pos = rank(sel.df$pos)
  update.selTable(sel.df, sel.row=new.row)

  #table = seminar.table(df=app$rdf)
  #setUI("studMainUI", HTML(table))
}

add.seminar.click = function(row, app=getApp(),se=app$se,...) {
  restore.point("add.seminar.click")

  sem.df = se$sem.df
  sel.df = se$sel.df
  # Seminar does already exist
  if (row %in% sel.df$row) return()
  #
  sel.df = rbind(sel.df,sem.df[row,])
  sel.df$pos = 1:NROW(sel.df)
  sem.df$selected[row] = TRUE
  update.selTable(sel.df,sel.row = NROW(sel.df))
  update.semTable(sem.df)

}

remove.seminar.click = function(pos,app=getApp(),se=app$se,...) {
  restore.point("remove.seminar.click")

  sem.df = se$sem.df
  sel.df = se$sel.df
  #
  row = sel.df$row[pos]
  sel.df = sel.df[-pos,]
  rows = sel.df$pos > pos
  sel.df$pos[rows] = sel.df$pos[rows]-1
  sem.df$selected[row] = FALSE
  update.selTable(sel.df,sel.row = NULL)
  update.semTable(sem.df)
}


joker.seminar.click = function(pos,app=getApp(),se=app$se,...) {
  sel.df = se$sel.df
  restore.point("joker.seminar.click")

  if (sel.df$joker[pos]) {
    sel.df$joker[pos] = FALSE
  } else {
    sel.df$joker = FALSE
    sel.df$joker[pos] = TRUE
  }
#  cat("\nsel.df: \n\n")
#  print(sel.df)

  update.selTable(sel.df=sel.df,sel.row = NULL)
}

save.studpref = function(app=getApp(), se=app$se,...) {
  restore.point("save.stud.prefs")

  dbBegin(se$db)
  dbDelete(se$db, "studpref",list(userid=se$userid, semester=se$semester))

  if (NROW(se$sel.df)>0) {
    sel.df = arrange(se$sel.df,pos)
    studpref = data_frame(semid=se$sem.df$semid[sel.df$row], userid=se$userid,semester=se$semester, pos=sel.df$pos, joker=sel.df$joker)
    dbInsert(se$db, "studpref",studpref, schema=app$glob$schemas$studpref)
  }
  dbCommit(se$db)
  createAlert(app$session, "saveStudprefAlert", title=NULL, content=app$glob$opts$rankingSaveSuccess[[app$lang]])
}
