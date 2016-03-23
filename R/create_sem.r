examples.EditSeminarApp = function() {
  setwd("D:/libraries/SeminarMatching/semedit_app/")
  db.dir = paste0(getwd(),"/db")

  restore.point.options(display.restore.point = TRUE)

  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())

  # Create Databases
  #create.login.db(db.arg = logindb.arg)
  #create.user.in.db(userid = "test", email = "sebkranz@gmail.com",password = "test",db.arg = logindb.arg)

  schema.file = "./schema/semdb.yaml"
  semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())
  #dbCreateSchemaTables(semdb, schema.file=schema.file)

  app = EditSeminarsApp(db.dir = db.dir, init.userid = "test", init.password="test", lang="de")

  runEventsApp(app, launch.browser = rstudio::viewer)

}

get.sem.number = function(semester) {
  year = as.numeric(substring(semester,3,4))
  year = year + 0.5*(tolower(substring(semester,1,1)) == "w")
  year
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

  form = load.and.init.form(file=paste0(yaml.dir,"/semform.yaml"), prefix="semform_")
  #form.schema.template(form)
  form$lang = lang
  form$widget.as.character=FALSE
  form$sets = glob$sets
  glob$semform = form

  form = load.and.init.form(file =paste0(yaml.dir,"/semcritform.yaml"),lang = lang,prefix = "semcrit_")
  form$sets = glob$sets
  glob$semcritform = form

  form = load.and.init.form(file =paste0(yaml.dir,"/semtopicsform.yaml"),lang = lang,prefix = "semtopic_")
  form$sets = glob$sets
  glob$semtopicsform = form

  form = load.and.init.form(file =paste0(yaml.dir,"/semstudform.yaml"),lang = lang,prefix = "semstud_")
  form$sets = glob$sets
  glob$semstudform = form


  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())


  login.fun = function(app=getApp(),userid,...) {
    restore.point("login.fun")

    se = new.env()
    se$db = app$glob$semdb
    se$userid = userid
    se$groups = dbGet(se$db,"groupstaff",list(userid=userid))

    # check if user is allowed to edit seminars
    if (NROW(se$groups)==0) {
      show.html.warning("semEditMainUI",paste0("The user ", userid, " has not been given any rights to edit seminars in any group."))
      return()
    } else if (sum(se$groups$edit_sem)==0) {
      show.html.warning("semEditMainUI",paste0("The user ", userid, " has not been given any rights to edit seminars in any group."))
      return()
    }
    se$groupid = se$groups$groupid[1]
    show.edit.sem.main(se=se)
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

  lop = loginPart(db.arg = logindb.arg, login.fun=login.fun, check.email.fun=check.email.fun, email.text.fun = email.text.fun, app.url=app.url, app.title=app.title,init.userid=init.userid, init.password=init.password,container.id = "semEditMainUI")
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
      uiOutput("semEditMainUI")
    )
  )
  app$lop = lop
  app
}


show.edit.sem.main = function(userid=se$userid, yaml.dir=app$glob$yaml.dir, db=app$glob$semdb, se=NULL, app=getApp(), semester=se[["semester"]]) {
  restore.point("show.edit.sem.main")

  if (is.null(semester)) {
    semester = get.default.semester(se=se)
    if (is.null(semester)) {
      semester = app$glob$sets$semesters[1]
    }
  }
  se$semester = semester

  se$seminars = dbGet(db,"seminars",list(groupid=se$groupid),schema=app$glob$schemas$seminars)
  se$seminars$locked = FALSE

  if (!is.null(se$seminars)) {
    # Activated and unactivated seminars
    se$aseminars = filter(se$seminars, semester==se$semester, active==TRUE)
    se$pseminars = filter(se$seminars, semester!=se$semester | active==FALSE)
    se$pseminars=se$pseminars[-get.sem.number(se$pseminars$semester),]
  }


  app$se = se

  atable = edit.seminar.table(se$aseminars, prefix="a")
  ptable = edit.seminar.table(se$pseminars, prefix="p")


  buttonHandler("createSeminarBtn",create.seminar.click)
  ui = fluidRow(column(width=10, offset=1,
    h4(paste0("Seminars for group ",se$groupid)),
    selectInput("semMainSemesterInput",label="Semester", choices=app$glob$sets$semesters, selected=se$semester),
    h5(paste0("Activated Seminars for ",se$semester)),
    HTML(atable),
    br(),
    actionButton("createSeminarBtn","Create Seminar"),
    h5(paste0("Unactivated seminars and previous seminars (can be used as templates)")),
    HTML(ptable)
  ))

  changeHandler("semMainSemesterInput",se=se, function(value,se,...) {
    show.edit.sem.main(se=se, semester=value)
  })

  setUI("semEditMainUI", ui)
}

add.edit.seminar.table.handler = function(rows,prefix="a") {
  editBtnId = paste0(prefix,"semEditBtn_",rows)
  copyEditBtnId = paste0(prefix,"semCopyEditBtn_",rows)

  for (i in seq_along(rows)) {
    row = rows[i]
    buttonHandler(editBtnId[i],edit.seminar.click,row=row, prefix=prefix, mode="edit",if.handler.exists = "skip")
    buttonHandler(copyEditBtnId[i],edit.seminar.click, row=row, prefix=prefix, mode="copyedit",if.handler.exists = "skip")
  }
}

edit.seminar.table = function(df = se$seminars, prefix="a", se=app$se, app=getApp()) {
  restore.point("edit.seminar.table")

  if (NROW(df)==0) {
    if (prefix == "a") {
      return("<p>... no seminars activated yet ...</p>")
    } else {
      return("<p>... no seminars ...</p>")
    }
  }

  rows = 1:NROW(df)
  add.edit.seminar.table.handler(rows, prefix=prefix)

  editBtnId = paste0(prefix,"semEditBtn_",rows)
  editBtns = extraSmallButtonVector(id=editBtnId, label="edit")

  copyEditBtnId = paste0(prefix,"semCopyEditBtn_",rows)
  copyEditBtns = extraSmallButtonVector(id=copyEditBtnId, label="new")

  deleteBtnId = paste0(prefix,"semDeleteBtn_",rows)
  if (prefix=="p") {
    deleteBtns = extraSmallButtonVector(id=copyEditBtnId, label="delete")
    deleteBtns[df$locked] = ""
  } else {
    deleteBtns = rep("", NROW(df))
  }

  btns = paste0(editBtns,copyEditBtns, deleteBtns, sep=" \n")

  cols = setdiff(colnames(df),c("groupid","locked","active"))
  wdf = data.frame(Action=btns, df[,cols])
  html.table(wdf, bg.color="#ffffff")
}


create.seminar.click=function(se = app$se, app=getApp(),...) {
  restore.point("create.seminar.click")

  se$seminar = empty.row.from.schema(app$glob$schemas$seminars, groupid=se$groupid, semester=se$semester, semester=se$semester)

  se$semcrit = empty.df.from.schema(app$glob$schemas$semcrit, 10)
  se$semcrit$semester = se$semester

  show.edit.seminar.ui(se=se, app=app)

}


edit.seminar.click=function(se = app$se, app=getApp(),mode="edit", prefix="a", row=1,...) {
  restore.point("edit.seminar.click")

  if (prefix=="a") {
    seminars = se$aseminars
  } else {
    seminars = se$pseminars
  }

  se$seminar = as.list(seminars[row,])

  se$semcrit = dbGet(se$db,"semcrit",list(semid=se$seminar$semid))

  if (NROW(se$semcrit)<10) {
    df = empty.df.from.schema(app$glob$schemas$semcrit, 10-NROW(se$semcrit), semid=se$seminar$semid)
    df$semester = se$semester
    se$semcrit = rbind(se$semcrit,df)
  }

  se$semtopic = dbGet(se$db,"semtopic",list(semid=se$seminar$semid))
  if (NROW(se$semtopic)<30) {
    df = empty.df.from.schema(app$glob$schemas$semtopic, 30-NROW(se$semtopic), semid=se$seminar$semid, semester=se$semester, size=1, userid=NA_character_)
    se$semtopic = rbind(se$semtopic,df)
  }
  se$semtopic$ind = 1:NROW(se$semtopic)



  if (mode=="copyedit") {
    se$seminar$semid = NA_integer_
    se$seminar$active = FALSE
    se$seminar$semester = se$semester
    se$seminar$locked = FALSE

    se$semcrit$semid = NA_integer_
    se$semcrit$semester = se$semester

  }

  se$semstuds = load.semstuds(se=se)

  show.edit.seminar.ui(se=se, app=app)
}


show.edit.seminar.ui = function(se, app=getApp(), edit=isTRUE(!is.na(se$seminar$semid))) {
  restore.point("show.edit.seminar.ui")

  glob = app$glob
  seminar = se$seminar

  form = glob$semform
  form.vals = form.default.values(glob$semform,values = seminar)
  form.ui = form.ui.simple(glob$semform, values=form.vals,add.submit = FALSE)

  topics.ui = crit.ui = NULL

  crit.df = table.form.default.values(glob$semcritform, data=se$semcrit)
  se$org.crit.df = crit.df
  crit.ui = form.ui.handsone.table(form = glob$semcritform,data = crit.df)

  top.df = table.form.default.values(glob$semtopicsform, data=se$semtopic)
  topics.ui = form.ui.handsone.table(form = glob$semtopicsform,data = top.df, stretchH="last")
  se$org.top.df = top.df


  if (edit) {
    header = h4("Edit Exististing Seminar")
  } else {
    header = h4("Create new seminar")
  }

  ui = fluidRow(column(width=10, offset=1,
    header,
    form.ui,
    crit.ui,
    topics.ui,
    br(),
    uiOutput("editSemAlert"),
    actionButton("saveSemBtn","Check and Save"),
    actionButton("exitEditSemBtn","Exit")
  ))
  clear.field.alert(id="editSemAlert")


  buttonHandler("saveSemBtn",save.sem.click)
  buttonHandler("exitEditSemBtn", function(...) show.edit.sem.main(se=se))

  pui = tabsetPanel(
    tabPanel("Seminar",ui),
    tabPanel("Assigned Students",uiOutput("semStudUI"))
  )
  show.semstud.ui(se=se)
  setUI("semEditMainUI",pui)
}

save.sem.click = function(se=app$se, app=getApp(),...) {
  glob  = app$glob

  sres = get.form.values(glob$semform)

  # We need the NULL value to return original table
  # if there were no changes to the table
  crit.df  = get.table.form.df(glob$semcritform, null.value = se$org.crit.df)
  top.df = get.table.form.df(glob$semtopicsform, null.value = se$org.top.df)

  restore.point("save.sem.click")
  if (!sres$ok) {
    show.field.alert(msg="Could not save, since not all fields are correctly entered.",id="editSemAlert")
    return()
  }

  if (is.null(se$seminar$enabled))
      se$seminar$enabled = TRUE
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
  crit.df$semester = se$semester

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

  res = try(dbDelete(se$db,"semtopic", list(semid=semid)))
  if (is(res,"try-error")) {
    dbRollback(se$db)
    msg = paste0("Error when updating database:<br> ",as.character(res))
    show.field.alert(msg=msg,id="editSemAlert")
    return()
  }

  if (!is.null(top.df)) {
    empty = is.na(top.df$topic) | nchar(str.trim(top.df$topic))==0
    top.df = top.df[!empty,]
    if (NROW(top.df)>0) {
      top.df$ind = 1:NROW(top.df)
      top.df$semid = semid
      top.df$semester = se$semester
      top.df$size = 1

      res = try(dbInsert(se$db,"semtopic",top.df,mode = "insert",schema=glob$schemas$semtopic))
      if (is(res,"try-error")) {
        dbRollback(se$db)
        msg = paste0("Error when updating database:<br> ",as.character(res))
        show.field.alert(msg=msg,id="editSemAlert")
        return()
      }
    }
  }

  dbCommit(se$db)

  show.field.alert(msg="Successfully saved.",id="editSemAlert", color=NULL)

}

show.semstud.ui = function(se, app=getApp()) {
  restore.point("show.semstud.ui ")

  glob = app$glob
  seminar = se$seminar

  if (NROW(se$semstuds)==0) {
    ui = fluidRow(column(width=10, offset=1,
      p("There are no students yet inscribed in the seminar.")
    ))
    setUI("semStudUI",ui)
    return()
  }

  tab.df = table.form.default.values(glob$semstudform, data=se$semstuds)
  se$org.semstuds.df = tab.df
  semstud.ui = form.ui.handsone.table(form = glob$semstudform,data = tab.df)



  ui = fluidRow(column(width=10, offset=1,
    semstud.ui
  ))

  setUI("semStudUI",ui)

}


load.semstuds = function(semid=se$seminar$semid, semester=se$semester,semtopic=se$semtopic,db=se$db, se=NULL) {
  restore.point("load.semstuds")

  if (is.null(semid)) return(NULL)

  ma = dbGet(db,"matchings", nlist(semid,semester))
  lc = dbGet(db,"latecomer", nlist(semid,semester))

  df = rbindlist(list(ma,lc),fill = TRUE)
  if (NROW(df)==0) return(NULL)

  df = select(df, semid,userid, semester)

  df = as_data_frame(df)
  if (is.null(semtopic)) {
    semtopic = dbGet(db,"semtopic",nlist(semid,semester))
  }

  if (is.null(semtopic)) {
    df$ind = NA
    df$topic = NA
  } else {
    df = left_join(df,select(semtopic,userid,ind,topic), by="userid")
  }

  userids = unique(df$userid)
  students = dbGet(db,sql = paste0("select * from students where userid in ",dplyr::escape(userids,parens = TRUE)," and semester = ", dplyr::escape(semester)))

  df = left_join(df, students, by=c("userid","semester"))
  df
}
