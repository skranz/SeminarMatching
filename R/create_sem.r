examples.EditSeminarApp = function() {
  setwd("D:/libraries/SeminarMatching/semedit_app/")
  db.dir = paste0(getwd(),"/db")

  restore.point.options(display.restore.point = TRUE)

  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())

  # Create Databases
  #create.login.db(db.arg = logindb.arg)
  #create.user.in.db(userid = "test", email = "sebkranz@gmail.com",password = "test",db.arg = logindb.arg)

  #schema.file = "./schema/semdb.yaml"
  #semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())
  #dbCreateSchemaTables(semdb, schema.file=schema.file)

  app = EditSeminarsApp(db.dir = db.dir, init.userid = "test", init.password="test", lang="de")

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

  glob$schemas = load.and.init.schemas(paste0(schema.dir, "/semdb.yaml"))
  glob$semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())

  glob$yaml.dir = yaml.dir
  glob$schema.dir = schema.dir
  glob$db.dir = db.dir

  glob$sets = read.yaml(file =paste0(yaml.dir,"/sets.yaml"), utf8 = TRUE)

  form = load.and.init.form(file=paste0(yaml.dir,"/semform.yaml"))
  #form.schema.template(form)
  form$lang = lang
  form$widget.as.character=FALSE
  form$sets = glob$sets
  glob$semform = form

  form = load.and.init.form(file =paste0(yaml.dir,"/semcritform.yaml"))
  #form.schema.template(form)
  form$lang = lang
  form$widget.as.character=FALSE
  form$sets = glob$sets
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
  ui = fluidRow(column(width=10, offset=1,
    HTML(table),
    br(),
    actionButton("createSeminarBtn","Create Seminar")
  ))
  setUI("mainUI", ui)
}


edit.seminar.table = function(df = se$seminars, se=app$se, app=getApp()) {
  restore.point("edit.seminar.table")

  if (NROW(df)==0) {
    return("<p>No seminars entered yet</p>")
  }

  rows = 1:NROW(df)
  editBtnId = paste0("semEditBtn_",rows)
  editBtns = extraSmallButtonVector(id=editBtnId, label="",icon=icon("pencil",lib = "glyphicon"))


  cols = colnames(df)
  wdf = data.frame(Edit=editBtns, df[,cols])
  html.table(wdf, bg.color="#ffffff")
}


create.seminar.click=function(se = app$se, app=getApp(),...) {
  restore.point("create.seminar.click")

  se$seminar = empty.row.from.schema(app$glob$schemas$seminars, groupid=se$groupid, semester=se$semester, semester=se$semester)

  se$semcrit = empty.df.from.schema(app$glob$schemas$semcrit, 10)


  show.edit.seminar.ui(se=se, app=app)

}

show.edit.seminar.ui = function(se, app=getApp()) {
  restore.point("show.edit.seminar.ui")

  glob = app$glob
  seminar = se$seminar

  form.vals = form.default.values(glob$semform,values = seminar)
  form.ui = form.ui.simple(glob$semform, values=form.vals,add.submit = FALSE)

  crit.df = table.form.default.values(glob$semcritform, data=se$semcrit)
  crit.ui = form.ui.handsone.table(form = glob$semcritform,data = crit.df)


  ui = fluidRow(column(width=10, offset=1,
    form.ui,
    crit.ui,
    br(),
    uiOutput("editSemAlert"),
    actionButton("saveSemBtn","Check and Save"),
    actionButton("exitEditSemBtn","Exit")
  ))
  clear.field.alert(id="editSemAlert")


  buttonHandler("saveSemBtn",save.sem.click)

  setUI("mainUI",ui)

#  form$success.handler = function(...) {
#    cat("\nGreat you inserted valid numbers!")
#  }

#  add.form.handlers(form)
}

save.sem.click = function(se=app$se, app=getApp(),...) {
  glob  = app$glob

  sres = get.form.values(glob$semform)

  crit.df  = get.table.form.df(glob$semcritform)

  restore.point("save.sem.click")
  if (!sres$ok) {
    show.field.alert(msg="Could not save, since not all fields are correctly entered.",id="editSemAlert")
    return()
  }

  se$seminar = copy.intersect(se$seminar,sres$values)

  dbBegin(se$db)
  # insert new seminar
  if (is.na(se$seminar$semid)) {
    res = try(dbInsert(se$db,"seminars",se$seminar,mode = "insert",schema=glob$schemas$seminars,get.key=TRUE))
  # update existing seminar
  } else {
    res = try(dbInsert(se$db,"seminars",se$seminar,mode = "replace", schema=glob$schemas$seminars))
  }

  if (is(res,"try-error")) {
    dbRollback(se$db)
    msg = paste0("Error when saving into database:<br> ",as.character(res))
    show.field.alert(msg=msg,id="editSemAlert")
    return()
  }
  se$seminar = res$values

  semid = se$seminar$semid

  crit.df$pos = 1:NROW(crit.df)
  crit.df$semid = semid


  #Rewrite criterion table
  res = try(dbDelete(se$db,"semcrit", list(semid=semid)))
  if (is(res,"try-error")) {
    dbRollback(se$db)
    msg = paste0("Error when updating database:<br> ",as.character(res))
    show.field.alert(msg=msg,id="editSemAlert")
    return()
  }

  res = try(dbInsert(se$db,"semcrit",crit.df,mode = "insert",schema=glob$schemas$semcrit))
  if (is(res,"try-error")) {
    dbRollback(se$db)
    msg = paste0("Error when updating database:<br> ",as.character(res))
    show.field.alert(msg=msg,id="editSemAlert")
    return()
  }

  se$semcrit = as_data_frame(res$values)

  dbCommit(se$db)

  show.field.alert(msg="Successfully saved.",id="editSemAlert", color=NULL)

}
