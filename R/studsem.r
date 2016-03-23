examples.StudSeminarsApp = function() {
  setwd("D:/libraries/SeminarMatching/semedit_app/")
  db.dir = paste0(getwd(),"/db")

  restore.point.options(display.restore.point = FALSE)

  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())
  app = StudSeminarsApp(db.dir = db.dir, init.userid = "test", init.password="test", lang="de")
  #runEventsApp(app, launch.browser = TRUE)

  runEventsApp(app, launch.browser = rstudio::viewer)

}


StudSeminarsApp = function(db.dir = paste0(getwd(),"/db"), schema.dir = paste0(getwd(),"/schema"), yaml.dir =  paste0(getwd(),"/yaml"), rmd.dir = paste0(getwd(),"/rmd"),   init.userid="", init.password="", app.title="Uni Ulm WiWi Seminar Selection", app.url = "http://localhost", email.domain = "uni-ulm.de", check.email.fun=NULL, email.text.fun=default.email.text.fun, use.db=TRUE, main.header=NULL, lang="en") {
  restore.point("StudSeminarsApp")

  library(loginPart)
  library(RSQLite)
  library(DBI)

  app = eventsApp()

  glob = app$glob

  glob$schemas = load.and.init.schemas(paste0(schema.dir, "/semdb.yaml"))
  glob$semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())

  glob$yaml.dir = yaml.dir
  glob$rmd.dir = rmd.dir
  glob$schema.dir = schema.dir
  glob$db.dir = db.dir

  glob$sets = read.yaml(file =paste0(yaml.dir,"/sets.yaml"), utf8 = TRUE)


  rmd.names = c("matchingnote","pre","post","round1","round2")
  glob$rmd.li = lapply(rmd.names, function(rmd.name) {
    file = paste0(glob$rmd.dir,"/studseminfo_",rmd.name,"_",lang,".Rmd")
    compile.rmd(file, out.type="shiny",use.commonmark = TRUE)
  })
  names(glob$rmd.li) = rmd.names

  glob$opts = opts = read.yaml(file=paste0(yaml.dir,"/settings.yaml"),keep.quotes = FALSE)
  glob$use_joker = isTRUE(opts$use_joker)
  lang = opts$start_lang

  texts = read.yaml(file=paste0(yaml.dir,"/texts.yaml"),keep.quotes = FALSE)
  glob$texts = lapply(texts,function(text) text[[lang]])


  # Set date format
  dateFormat = glob$texts[["dateFormat"]]
  .GlobalEnv$knit_print.Date =  function(x,...) {
    format(x, format="%a. %d.%m.%Y")
  }

  app$opts = glob$opts
  app$lang = lang

  form = load.and.init.form(file=paste0(yaml.dir,"/studform.yaml"), lang=lang, prefix="studform_")
  #form.schema.template(form)

  form$sets = glob$sets
  glob$studform = form

  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())

  login.fun = function(app=getApp(),userid,...) {
    se = refresh.stud.app.data(userid=userid)
    ui = tabsetPanel(
      id = "studTabsetPanel",
      tabPanel(title = app$glob$texts$studsemTab, value="semPanel", uiOutput("studsemUI")),
      tabPanel(title = app$glob$texts$studtopicTab, value="topicPanel", uiOutput("studtopicsUI")),
      tabPanel(title = app$glob$texts$studstudTab, value="studPanel", uiOutput("studformUI"))
    )
    setUI("studMainUI", ui)

    if (!isTRUE(se$stud.exists)) {
      updateTabsetPanel(session=app$session, "studTabsetPanel", selected="studPanel")
    }
    #show.stud.info.ui()
    show.stud.form.ui()
    show.stud.sem.ui()
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

  lop = loginPart(db.arg = logindb.arg, login.fun=login.fun, check.email.fun=check.email.fun, email.text.fun = email.text.fun, app.url=app.url, app.title=app.title,init.userid=init.userid, init.password=init.password,container.id = "studMainUI")
  set.lop(lop)
  lop.connect.db(lop=lop)
  lop$login$ui = lop.login.ui(lop)
  lop$smtp = lop.get.smtp()

  appInitHandler(function(session,...) {

    initLoginDispatch(lop)
  })

  app$ui = tagList(
    fluidPage(
      uiOutput("studMainUI")
    )
  )

  app$lop = lop
  app
}

load.student.from.db = function(userid=se$userid, semester=NULL, app=getApp(), se=app$se) {
  restore.point("load.student.from.db")

  all = dbGet(se$db,"students",list(userid=userid),
    schema=app$glob$schemas$students)

  if (NROW(all)==0) return(list(userid=userid, email=userid, semester=se$semester))

  .sem = semester
  stud = filter(all, semester==.sem)
  if (NROW(stud)>0) {
    se$stud.exists = TRUE
    se$stud = stud
    return(stud)
  }

  sem.num = get.sem.number(all$semester)
  .sem = all$semester[which.max(sem.num)]
  stud = filter(all, semester==.sem)[1,]

  se$stud.exists = FALSE
  se$stud = stud
  return(stud)
}

refresh.stud.app.data = function(userid=se$userid, se=NULL, app=getApp()) {
  restore.point("refresh.stud.app.data")

  if (is.null(se)) {
    se = new.env()
    se$db = app$glob$semdb
    se$userid = userid
  }

  semester = get.default.semester(se=se)
  if (is.null(semester)) {
    semester = app$glob$sets$semesters[1]
  }
  se$semester = semester

  app$se = se
  se$semester = semester
  se$use_joker = app$glob$use_joker

  se$stud = load.student.from.db(userid=userid, semester=semester,se=se)

  se$seminars = dbGet(se$db,"seminars",list(semester=semester,active=TRUE),schema=app$glob$schemas$seminars)
  se$seminars = mutate(se$seminars, free_slots = slots-filled_slots)

  se$studpref = dbGet(se$db,"studpref", list(userid=userid, semester=semester), schema=app$glob$schemas$studpref, orderby="pos ASC")


  admin = dbGet(se$db,"admin",nlist(semester=semester),schema=app$glob$schemas$admin)
  se$admin = init.se.admin(admin)

  se$studmode = se$admin$studmode

  se$assign = dbGet(se$db,"assign", nlist(semester),schema=app$glob$schemas$assign)

  if (NROW(se$assign)>0) {
    stud_sems =
      filter(se$assign, userid==se$userid) %>%
      arrange(assign_time)
    if (NROW(stud_sems)==0) {
      se$stud_sems=NULL
    } else {
      se$stud_sems = left_join(stud_sems,se$seminars, by=c("semid","semester"),copy=TRUE)
    }
  } else {
    se$stud_sems = NULL
  }



  se

}

show.stud.form.ui = function(se=app$se, app=getApp()) {

  restore.point("show.stud.form.ui")

  glob = app$glob
  form = glob$studform
  form.vals = form.default.values(glob$studform,values = se$stud)
  form.ui = form.ui.simple(glob$studform, values=form.vals,add.submit = TRUE)
  clear.form.alert(form=form)

  add.form.handlers(form,success.handler = save.studform)


  ui = c(list(HTML(app$glob$texts$profileIntro)), form.ui)
  setUI("studformUI", ui)

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

show.stud.sem.ui = function(se=app$se, app=getApp()) {
  restore.point("show.stud.sem.ui")

  studmode = se$studmode
  if (studmode=="post") {
    show.stud.sem.post.ui(se,app)
  } else if (studmode=="round1" | studmode=="round2") {
    show.stud.sem.round.ui(se,app)
  } else {
    show.stud.sem.pre.ui(se,app)
  }

}

show.stud.sem.pre.ui = function(se=app$se, app=getApp()) {
  restore.point("show.stud.sem.pre.ui")
  studmode = "pre"
  envir = c(se$admin, list(stud_sems,se$stud_sems,stud.exists=se$stud.exists))
  cr = app$glob$rmd.li[[studmode]]
  header = render.compiled.rmd(cr, envir=envir)
  ui = fluidRow(column(offset=1, width=10,
    header
  ))
  setUI("studsemUI", ui)
}


show.stud.sem.post.ui = function(se=app$se, app=getApp()) {
  restore.point("show.stud.sem.post.ui")
  studmode = "post"
  envir = c(se$admin, nlist(stud_sems=se$stud_sems,stud.exists=se$stud.exists))

  cr = app$glob$rmd.li[[studmode]]
  header = render.compiled.rmd(cr, envir=envir)
  ui = fluidRow(column(offset=1, width=10,
    header
  ))
  setUI("studsemUI", ui)

}


show.stud.sem.round.ui = function(se=app$se, app=getApp()) {
  restore.point("show.stud.sem.round.ui")

  studmode = se$studmode
  if (studmode=="round2")  {
    round = 2
  } else {
    round = 1
  }
  lang = app$lang
  glob = app$glob
  opts = glob$opts
  sems = se$seminars

  envir = c(se$admin, nlist(stud_sems=se$stud_sems, stud.exists=se$stud.exists))
  cr = glob$rmd.li[[studmode]]
  header = render.compiled.rmd(cr, envir=envir)
  note = render.compiled.rmd(glob$rmd.li[["matchingnote"]], envir=se)

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
    header,
    note,
    h3(glob$texts$selSemTitle),
    uiOutput("selSemUI"),
    br(),
    actionButton("saveStudprefBtn",glob$texts$rankingSaveBtnLabel),
    bsAlert("saveStudprefAlert"),
    h3(glob$texts$allSemTitle),
    uiOutput("allSemUI")
  ))


  #view.ui(header)
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
  createAlert(app$session, "saveStudprefAlert", title=NULL, content=app$glob$texts$rankingSaveSuccess)
}

max.date = function(vals) {
  m = suppressWarnings(max(vals, na.rm=TRUE))
  if (!is.finite(m)) m = as.Date(NA)
  m
}
