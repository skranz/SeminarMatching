examples.EditSeminarApp = function() {
  setwd("D:/libraries/SeminarMatching/semedit_app/")
  db.dir = paste0(getwd(),"/db")

  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())

  # Create Databases
  #create.login.db(db.arg = logindb.arg)
  #create.user.in.db(userid = "test", email = "sebkranz@gmail.com",password = "test",db.arg = logindb.arg)

  #schema.file = "./schema/semdb.yaml"
  #semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())
  #dbCreateSchemaTables(semdb, schema.file=schema.file)

  app = EditSeminarsApp(db.dir = db.dir, init.userid = "test", init.password="test")

  runEventsApp(app, launch.browser = rstudio::viewer)

}

EditSeminarsApp = function(db.dir = paste0(getwd(),"/db"), schema.dir = paste0(getwd(),"/schema"), yaml.dir =  paste0(getwd(),"/yaml"),   init.userid="", init.password="", app.title="Uni Ulm WiWi Seminar Editor", app.url = "http://localhost", email.domain = "uni-ulm.de", check.email.fun=NULL, email.text.fun=default.email.text.fun, use.db=TRUE, main.header=NULL, lang="en") {
  restore.point("EditSeminarsApp")

  library(shinyjs)
  library(loginPart)
  library(RSQLite)
  library(DBI)

  app = eventsApp()

  glob = app$glob

  glob$schemas = yaml.load_file(paste0(schema.dir, "/semdb.yaml"))
  glob$semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())

  glob$yaml.dir = yaml.dir
  glob$schema.dir = schema.dir
  glob$db.dir = db.dir

  glob$sets = read.yaml(file =paste0(yaml.dir,"/sets.yaml"), utf8 = TRUE)
  form = read.yaml(file =paste0(yaml.dir,"/semform.yaml"),utf8 = TRUE)
  form$lang = lang
  form$widget.as.character=FALSE
  glob$semform = form

  form = read.yaml(file =paste0(yaml.dir,"/semcritform.yaml"),utf8 = TRUE)
  form$lang = lang
  form$widget.as.character=FALSE
  glob$semcritform = form

  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())


  login.fun = function(app=getApp(),userid,...) {
    se = show.edit.sem.main(userid=userid)
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

  lop = loginPart(db.arg = logindb.arg, login.fun=login.fun, check.email.fun=check.email.fun, email.text.fun = email.text.fun, app.url=app.url, app.title=app.title,init.userid=init.userid, init.password=init.password,container.id = "mainUI")
  set.lop(lop)
  lop.connect.db(lop=lop)
  lop$login$ui = lop.login.ui(lop)
  lop$smtp = lop.get.smtp()

  appInitHandler(function(session,...) {
    initLoginDispatch(lop)
  })

  jsCode <- "shinyjs.openLink = function(url){window.open(url,'_blank');}"
  app$ui = tagList(
    useShinyjs(),
    extendShinyjs(text = jsCode),
    fluidPage(
      uiOutput("mainUI")
    )
  )
  app$lop = lop
  app
}


show.edit.sem.main = function(userid, yaml.dir=app$glob$yaml.dir, db=app$glob$semdb, se=NULL, app=getApp()) {
  semester = "SS15"
  restore.point("show.edit.sem.main")

  if (is.null(se)) {
    se = new.env()
    se$semester = semester
    se$db = db
    se$userid = userid
    se$groups = dbGet(db,"groupstaff",userid=userid)

    # check if user is allowed to edit seminars
    if (NROW(se$groups)==0) {
      show.html.warning("mainUI",paste0("The user ", userid, " has not been given any rights to edit seminars in any group."))
      return()
    } else if (sum(se$groups$edit_sem)==0) {
      show.html.warning("mainUI",paste0("The user ", userid, " has not been given any rights to edit seminars in any group."))
      return()
    }

    se$groupid = se$groups$groupid[1]
    se$seminars = dbGet(db,"seminars",groupid=se$groupid)
  }

  app$se = se

  table = edit.seminar.table(se$seminars)


  buttonHandler("createSeminarBtn",create.seminar.click)
  ui = fluidRow(width=10, offset=1,
    HTML(table),
    br(),
    actionButton("createSeminarBtn","Create Seminar")
  )
  setUI("mainUI", ui)
}

#' Creates an example row from a database schema table
#' using provided column values and default values specified in schema
empty.row.from.schema = function(.schema, ..., .use.defaults = TRUE) {
  restore.point("schema.value.list")

  table = .schema$table
  vals = replicate(length(table),NA, simplify=FALSE)
  names(vals) = names(table)
  if (.use.defaults & !is.null(.schema$defaults)) {
    vals[names(.schema$defaults)] = .schema$defaults
  }
  args = list(...)
  vals[names(args)] = args
  vals
}

#' Creates an example data frame from a database schema table
#' using provided column values and default values specified in schema
empty.df.from.schema = function(.schema,.nrows=1, ..., .use.defaults = TRUE) {
  restore.point("empty.df.from.schema")
  li = empty.row.from.schema(.schema, ..., .use.defaults=.use.defaults)
  if (.nrows==1) return(as.data.frame(li))

  df = as.data.frame(lapply(li, function(col) rep(col,length.out = .nrows)))
  df
}


edit.seminar.table = function(df = se$seminars, se=app$se, app=getApp()) {
  restore.point("edit.seminar.table")

  if (NROW(df)==0) {
    return("<p>No seminars entered yet</p>")
  }

  rows = 1:NROW(df)
  editBtnId = paste0("semEditBtn_",rows)
  editBtns = extraSmallButtonVector(id=starBtnId, label="",icon=icon("pencil",lib = "glyphicon"))


  cols = colnames(df)
  wdf = data.frame(Edit=editBtns, df[,cols])
  html.table(wdf, bg.color="#ffffff")
}


create.seminar.click=function(se = app$se, app=getApp(),...) {
  restore.point("create.seminar.click")

  se$seminar = empty.row.from.schema(app$glob$schemas$seminars, groupid=se$groupid, semester=se$semester)

  se$semcrit = empty.df.from.schema(app$glob$schemas$semcrit, 10)


  show.edit.seminar.ui(se=se, app=app)

}

edit.seminar.ui = function(se, app=getApp()) {
  restore.point("show.edit.seminar.ui")

  glob = app$glob
  seminar = se$seminar

  vals1 = form.default.values(glob$semform,values = seminar)
  ui1 = form.ui.simple(glob$semform, values=vals1,submitBtn = FALSE)





  form$success.handler = function(...) {
    cat("\nGreat you inserted valid numbers!")
  }

  set.form(form)
  ui = form.ui.simple(form)

  add.form.handlers(form)
  app$ui = fluidPage(ui)
  runEventsApp(app, launch.browser = rstudio::viewer)
}
