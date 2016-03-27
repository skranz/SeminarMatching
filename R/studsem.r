examples.StudSeminarsApp = function() {
  setwd("D:/libraries/SeminarMatching/semapps/shared")
  restore.point.options(display.restore.point = FALSE)
  app = StudSeminarsApp(init.userid = "test", init.password="test", lang="de")
  viewApp(app)

}


StudSeminarsApp = function(db.dir = paste0(main.dir,"/db"), schema.dir = paste0(main.dir,"/schema"), yaml.dir =  paste0(main.dir,"/yaml"), rmd.dir = paste0(main.dir,"/rmd"), main.dir=getwd(),   init.userid="", init.password="", app.title="Uni Ulm WiWi Seminar Selection", app.url = "http://localhost", email.domain = "uni-ulm.de", check.email.fun=NULL, email.text.fun=default.email.text.fun, use.db=TRUE, main.header=NULL, lang="en") {
  restore.point("StudSeminarsApp")

  library(loginPart)
  library(RSQLite)
  library(DBI)

  app = eventsApp()

  glob = app$glob

  glob$schemas = load.and.init.schemas(paste0(schema.dir, "/semdb.yaml"))
  glob$semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())

  glob$main.dir = main.dir
  glob$yaml.dir = yaml.dir
  glob$rmd.dir = rmd.dir
  glob$schema.dir = schema.dir
  glob$db.dir = db.dir

  glob$sets = read.yaml(file =paste0(yaml.dir,"/sets.yaml"), utf8 = TRUE)


  rmd.names = c("pre","post","round1","round2")
  rmd.names = c(
    paste0("studseminfo_",rmd.names),
    paste0("stud_overview_",rmd.names),
    paste0("studseminfo_matchingnote")
  )

  glob$rmd.li = lapply(rmd.names, function(rmd.name) {
    file = paste0(glob$rmd.dir,"/",rmd.name,"_",lang,".Rmd")
    compile.rmd(file, out.type="shiny",use.commonmark = TRUE, fragment.only = TRUE)
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
    app$se = refresh.stud.app.data(userid=userid)
    ui = tabsetPanel(
      id = "studTabsetPanel",
      tabPanel(title = app$glob$texts$studoverviewTab, value="overviewPanel", uiOutput("overviewUI")),
      tabPanel(title = app$glob$texts$studsemTab, value="semPanel", uiOutput("studsemUI")),
      tabPanel(title = app$glob$texts$studtopicTab, value="topicPanel", uiOutput("studtopicsUI")),
      tabPanel(title = app$glob$texts$studstudTab, value="studPanel", uiOutput("studformUI"))
    )
    setUI("studMainUI", ui)
    show.stud.overview.ui()
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

  # Select the assigned seminars of the student
  sql = "
  SELECT * FROM assign
  NATURAL LEFT JOIN semtopic
  NATURAL LEFT JOIN seminars
  WHERE (assign.userid = :userid AND
        assign.semester = :semester)
  "
  se$stud_sems = dbGet(se$db, sql=sql, params=nlist(semester, userid))


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

show.stud.overview.ui = function(se=app$se, app=getApp()) {
  restore.point("show.stud.overview.ui")

  studmode = se$studmode

  envir = c(se$admin, list(stud_sems = se$stud_sems,stud.exists=se$stud.exists))
  cr = app$glob$rmd.li[[paste0("stud_overview_",studmode)]]
  header = render.compiled.rmd(cr, envir=envir)
  ui = fluidRow(column(offset=1, width=10,
    header
  ))
  setUI("overviewUI", ui)

}

assigned.sems.table = function(stud_sems=se$stud_sems,cols=c("semname","teacher"), header=cols, topic.header="topic",topic.choice.label="Choose Topic",se=app$se, app=getApp()) {

  restore.point("assigned.sems.table")
  cols = unique(c(cols,"topic"))
  header = c(header, topic.header)

  df = stud_sems[,cols, drop=FALSE]

  df$semname = as.weblink(link=stud_sems$weblink, label=df$semname)

  topicBtns = extraSmallButtonVector(id=paste0("topicBtn",1:NROW(df)),label=topic.choice.label)
  rows = is.na(stud_sems$topic) & !is.na(stud_sems$topics_date)
  df$topic[rows] = topicBtns[rows]
  rows = is.na(df$topic)
  df$topic[rows] = "---"

  html.table(df,header = header,bg.color = "#ffffff")
}

as.weblink = function(link, label, target=' targe="_blank"') {
  restore.point("as.weblink")

  if (length(link)==0) return(NULL)
  has.link = nchar(link) > 0
  str = paste0("<a href='",link,"'",target,">",label,"</a>")
  str[!has.link] = label[!has.link]
  str
}

show.stud.sem.ui = function(se=app$se, app=getApp()) {
  restore.point("show.stud.sem.ui")

  compute.sem.df(se=se)
  studmode = se$studmode

  envir = c(se$admin, nlist(stud_sems=se$stud_sems, stud.exists=se$stud.exists,se=se))
  cr = app$glob$rmd.li[[paste0("studseminfo_",studmode)]]
  ui = render.compiled.rmd(cr, envir=envir,out.type = "shiny",fragment.only = TRUE)

  buttonHandler("saveStudprefBtn",save.studpref)
  add.studpref.handlers(num.sems=NROW(se$sem.df))

  setUI("studsemUI", ui)
  show.selsem.table(se=se)
  show.sem.table(se=se)

}

compute.sem.df = function(se=app$se, app=getApp(), opts=app$opts) {
  restore.point("compute.sem.df")

  studmode = se$studmode
  if (studmode=="round2")  {
    round = 2
  } else {
    round = 1
  }

  sems = se$seminars
  cols = c("semid",intersect(union(opts$selSemCols,opts$allSemCols),colnames(sems)))
  sem.df = sems[,cols]

  # Add links to seminar titles
  if ("weblink" %in% colnames(sems)) {
    sem.df$semname = as.weblink(link = sems$weblink,label = sem.df$semname)
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
    sem.df$selected[sel.rows] = TRUE
  } else {
    sel.rows = integer(0)
    sel.df = sem.df[sel.rows,]
  }

  se$sem.df = sem.df
  se$sel.df = sel.df
}

show.stud.sem.round.ui = function(se=app$se, app=getApp()) {
  restore.point("show.stud.sem.round.ui")
  lang = app$lang
  glob = app$glob
  opts = glob$opts

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


show.selsem.table = function(sel.df=se$sel.df, sel.row=NULL, app=getApp(), se=app$se, opts=app$opts, lang=app$lang, header = opts$selSemColnames[[lang]], cols=app$opts$selSemCols) {
  restore.point("show.selsem.table")

  if (NROW(sel.df)==0) {
    setUI("selSemUI",p("---"))
    return()
  }

  widget.df = sel.widgets.df(sel.df, cols=cols)

  if (!app$glob$use_joker) {
    header = setdiff(header, c("joker","Joker"))
  }

  html = html.table(widget.df,sel.row = sel.row, header=header, bg.color="#ffffff")

  setUI("selSemUI",HTML(html))

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


show.sem.table = function(sem.df=se$sem.df, sel.rows=which(sem.df$selected), app=getApp(), se=app$se, header=app$opts$allSemColnames[[app$lang]], cols=app$opts$selSemCols) {
  restore.point("show.sem.table")

  sem.df$selected[sel.rows] = TRUE

  se$sem.df = sem.df

  widget.df = sem.widgets.df(sem.df, cols=cols)
  html =   html.table(widget.df,sel.row = sel.rows,header=header , bg.color="#ffffff", sel.color="#aaffaa")
  setUI("allSemUI",HTML(html))
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
  se$sel.df = sel.df
  show.selsem.table(sel.df, sel.row=new.row)

}

add.seminar.click = function(row, app=getApp(),se=app$se,...) {
  restore.point("add.seminar.click")
  cat("\nadd seminar click")
  # Seminar does already exist
  if (row %in% se$sel.df$row) return()
  #
  se$sel.df = rbind(se$sel.df,se$sem.df[row,])
  se$sel.df$pos = 1:NROW(se$sel.df)
  se$sem.df$selected[row] = TRUE

  show.selsem.table(se=se,sel.row = NROW(se$sel.df))
  show.sem.table(se=se)

}

remove.seminar.click = function(pos,app=getApp(),se=app$se,...) {
  restore.point("remove.seminar.click")
  cat("\nremove seminar click")

  sem.df = se$sem.df
  sel.df = se$sel.df
  #
  row = sel.df$row[pos]
  sel.df = sel.df[-pos,]
  rows = sel.df$pos > pos
  sel.df$pos[rows] = sel.df$pos[rows]-1
  sem.df$selected[row] = FALSE
  se$sem.df = sem.df
  se$sel.df = sel.df

  show.selsem.table(se=se,sel.row = NULL)
  show.sem.table(se=se)
}


joker.seminar.click = function(pos,app=getApp(),se=app$se,...) {
  restore.point("joker.seminar.click")

  if (sel.df$joker[pos]) {
    se$sel.df$joker[pos] = FALSE
  } else {
    se$sel.df$joker = FALSE
    se$sel.df$joker[pos] = TRUE
  }
  show.selsem.table(se=se,sel.row = NULL)
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
