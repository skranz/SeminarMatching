examples.AdminSeminarApp = function() {
  setwd("D:/libraries/SeminarMatching/semedit_app/")
  db.dir = paste0(getwd(),"/db")

  restore.point.options(display.restore.point = TRUE)

  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())

  app = AdminSeminarsApp(db.dir = db.dir, init.userid = "test", init.password="test", lang="en")

  runEventsApp(app, launch.browser = rstudio::viewer)

}

AdminSeminarsApp = function(db.dir = paste0(getwd(),"/db"), schema.dir = paste0(getwd(),"/schema"), yaml.dir =  paste0(getwd(),"/yaml"),   init.userid="", init.password="", app.title="Uni Ulm WiWi Seminar Administration", app.url = "http://localhost", email.domain = "uni-ulm.de", check.email.fun=NULL, email.text.fun=default.email.text.fun, use.db=TRUE, main.header=NULL, lang="en") {
  restore.point("AdminSeminarsApp")

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

    ui = tabsetPanel(
      tabPanel(title = "Setting", uiOutput("semAdminSettingsUI")),
      tabPanel(title = "Seminars & Matching", uiOutput("semAdminSemUI"))
    )
    setUI("semAdminMainUI",ui)

    show.admin.main(se=se)
    show.admin.sems(se=se)
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

  se$admin = dbGet(se$db,"admin")
  if (is.null(semester)) {
    if (NROW(se$admins)>0) {
      semester = max(se$admin)
    } else {
      semester = app$glob$sets$semesters[[1]]
    }
  }
  se$semester = semester
  se$seminars = dbGet(se$db,"seminars", list(semester=se$semester))

  se$ses = se$admin[se$admin$semester==se$semester,,drop=FALSE]
  se$studpref = dbGet(se$db,"studpref",list(semester=se$semester))
  se$students = dbGet(se$db,"students",list(semester=se$semester))

  if (NROW(se$ses)==0) se$ses = list(semester=se$semester)

  return(invisible())
}


show.admin.main = function(userid=se$userid, yaml.dir=app$glob$yaml.dir, db=app$glob$semdb, se, semester=se$semester, app=getApp(), refresh=TRUE) {

  restore.point("show.admin.main")

  se$semester = semester
  if (refresh)
    load.admin.data.from.db(se=se)

  glob = app$glob
  form = glob$adminform
  form.vals = form.default.values(form,values = se$ses)
  form.ui = form.ui.simple(form, values=form.vals,add.submit = TRUE)
  clear.form.alert(form=form)


  add.form.handlers(form,success.handler = save.adminform)

  changeHandler("semester", function(value,...) {
    show.admin.main(se=se, semester=value, refresh=TRUE)
    show.admin.sems(se=se)
  })

  ui = fluidRow(column(width=10, offset=1,
    form.ui
  ))

  setUI("semAdminSettingsUI", ui)

}

save.adminform = function(values, app=getApp(), se=app$se,...) {
  restore.point("save.studForm")

  #schema.template(values, "students")
  form = app$glob$adminform

  se$ses[names(values)] = values

  dbBegin(se$db)
  dbDelete(se$db,"admin", se$ses["semester"])
  dbInsert(se$db,"admin", se$ses)
  dbCommit(se$db)

  show.form.alert(form=form,msg=form$texts$submitSuccess, color=NULL)
  show.admin.main(se=se)
}

show.admin.sems = function(se=app$se, app=getApp()) {
  restore.point("show.admin.sems")

  se$asems = filter(se$seminars,active==TRUE)
  df = se$asems

  if (NROW(df)>0) {
    rows = 1:NROW(df)
    endisBtnId = paste0("endisBtn_",rows)
    endisLabel = ifelse (df$enabled, "disable", "enable")

    endisBtns = extraSmallButtonVector(id=endisBtnId,label=endisLabel)
    cols = setdiff(colnames(df), c("semester","active","enabled"))
    wdf = data.frame(action=endisBtns,df[,cols])
    html = html.table(wdf, sel.row=which(!df$enabled), sel.col="#aaaaaa",bg.col="#ffffff")

  } else {
    html="<p>No active seminars for this semester yet.</p>"
  }

  num.pref = length(unique(se$studpref$userid))



  ui = fluidRow(column(offset=1, width=10,
    h4(paste0("Seminars for ", se$semester)),
    br(),
    HTML(html),
    br(),
    p(paste0(num.pref, " students have submitted their preferences.")),
    actionButton("doMatching1Btn","Perform Round 1 Matching")
  ))
  setUI("semAdminSemUI", ui)

}

