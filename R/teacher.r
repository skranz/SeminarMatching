

examples.EditSeminarApp = function() {
  setwd("D:/libraries/SeminarMatching/semapps/shared")
  app = EditSeminarsApp(init.userid = "test", init.password="test", lang="de")
  viewApp(app)

}

example.create.db = function() {
  setwd("D:/libraries/SeminarMatching/semapps/shared")
  db.dir = paste0(getwd(),"/db")

  restore.point.options(display.restore.point = TRUE)

  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())

  # Create Databases
  #create.login.db(db.arg = logindb.arg)
  #create.user.in.db(userid = "test", email = "sebkranz@gmail.com",password = "test",db.arg = logindb.arg)

  schema.file = "./schema/semdb.yaml"
  semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())
  dbCreateSchemaTables(semdb, schema.file=schema.file,overwrite = FALSE)


}

get.sem.number = function(semester) {
  year = as.numeric(substring(semester,3,4))
  year = year + 0.5*(tolower(substring(semester,1,1)) == "w")
  year
}

EditSeminarsApp = function(db.dir = paste0(main.dir,"/db"), schema.dir = paste0(main.dir,"/schema"), yaml.dir =  paste0(main.dir,"/yaml"), rmd.dir =  paste0(main.dir,"/rmd"), report.dir =  paste0(main.dir,"/reports"), main.dir=getwd(),   init.userid="", init.password="", app.title="Uni Ulm WiWi Seminar Editor", app.url = "http://localhost", email.domain = "uni-ulm.de", check.email.fun=NULL, email.text.fun=default.email.text.fun, use.db=TRUE, main.header=NULL, lang="en") {
  restore.point("EditSeminarsApp")

  library(shinyjs)
  library(loginPart)
  library(RSQLite)
  library(DBI)

  app = eventsApp()

  app$num.edit.seminar.table.handler = c("a"=0,"p"=0)
  glob = app$glob

  glob$schemas = load.and.init.schemas(paste0(schema.dir, "/semdb.yaml"))
  glob$semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())

  glob$yaml.dir = yaml.dir
  glob$schema.dir = schema.dir
  glob$db.dir = db.dir
  glob$rmd.dir = rmd.dir
  glob$report.dir = report.dir

  glob$cur_admin = get.current.admin(main.dir=main.dir)
  glob$sets = read.yaml(file =paste0(yaml.dir,"/sets.yaml"), utf8 = TRUE)

  glob$semesters.with.matchings = get.semesters.that.have.matchings(db=glob$semdb)

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


  rmd.names = c("teacher_overview")
  glob$rmd.li = lapply(rmd.names, function(rmd.name) {
    restore.point("snhfbhuefburbfubruu")

    file = paste0(glob$rmd.dir,"/",rmd.name,"_",lang,".Rmd")
    compile.rmd(file=file, out.type="html",use.commonmark = TRUE, fragment.only = TRUE)
  })
  names(glob$rmd.li) = rmd.names


  # Init reports
  report.dir = app$glob$report.dir
  file = paste0(report.dir,"/matching_sem.Rmd")
  rmd = readLines(file,warn = FALSE)
  rmd = remove.rmd.chunks(rmd, "init_param")
  rmd = paste0(rmd, collapse="\n\n")
  glob$reports.rmd = list("matchin_sem"=rmd)


  logindb.arg = list(dbname=paste0(db.dir,"/loginDB.sqlite"),drv=SQLite())


  login.fun = function(app=getApp(),userid,...) {
    restore.point("login.fun")

    se = new.env()
    app$se = se
    se$db = app$glob$semdb
    se$userid = userid
    se$groups = dbGet(se$db,"groupstaff",list(userid=userid))
    se$semester = glob$cur_admin$semester
    se$cur_semester = se$semester
    se$admin = glob$cur_admin


    if (is.null(se$admin)) {
      show.html.warning("mainUI",paste0("Basic administration data is missing that specifies the current semester. The administrator has to add it before seminars can be specified."))
      return()
    }
    # check if user is allowed to edit seminars
    if (NROW(se$groups)==0) {
      show.html.warning("mainUI",paste0("The user ", userid, " has not been given any rights to edit seminars in any group."))
      return()
    } else if (sum(se$groups$edit_sem)==0) {
      show.html.warning("mainUI",paste0("The user ", userid, " has not been given any rights to edit seminars in any group."))
      return()
    }
    se$groupid = se$groups$groupid[1]
    setUI("mainUI",
      tabsetPanel(id = "mainTabset",
        tabPanel("Info",value = "infoTab", uiOutput("overviewUI")),
        tabPanel("Seminar", value="semTab", uiOutput("semEditUI")),
        tabPanel("Assignment", value="assignTab", uiOutput("assignUI")),
        tabPanel("Report", value="reportTab", uiOutput("reportUI"))
      )
    )

    load.teacher.se(semester=se$semester)
    show.edit.sem.main(se=se)
    show.teacher.overview(se=se)
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

  changeHandler("semMainSemesterInput", function(value,...) {
    semester = value
    load.teacher.se(semester=semester)
    show.edit.sem.main()
  })

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

load.teacher.se = function(semester=se$semester,db=app$glob$semdb, app=getApp(), se=app$se) {
  restore.point("load.teacher.se")


  se$semester = semester
  se$seminars = dbGet(db,"seminars",list(groupid=se$groupid),schema=app$glob$schemas$seminars)


  if (!is.null(se$seminars)) {
    se$seminars$locked =se$seminars$semester %in% app$glob$semesters.with.matchings

    # Activated and unactivated seminars
    se$aseminars = filter(se$seminars, semester==se$semester, active==TRUE)
    se$pseminars = filter(se$seminars, semester!=se$semester | active==FALSE)
    se$pseminars=se$pseminars[-get.sem.number(se$pseminars$semester),]
  }

  se$admin = get.current.admin(semester=se$semester)

  se$today = as.Date(Sys.time())
  se$has.assignment = !is.na(se$admin$round1_done_date) | !is.na(se$admin$round2_done_date)

  app$se = se

}

show.edit.sem.main = function(userid=se$userid, yaml.dir=app$glob$yaml.dir, db=app$glob$semdb, se=app$se, app=getApp(), semester=se[["semester"]]) {
  restore.point("show.edit.sem.main")


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



  setUI("semEditUI", ui)
}

add.edit.seminar.table.handler = function(rows,prefix="a", app=getApp()) {
  restore.point("add.edit.seminar.table.handler")

  n = length(rows)
  if (isTRUE(app$num.edit.seminar.table.handler[prefix] >= n))
    return()

  if (!is.null(app$num.edit.seminar.table.handler[prefix]))
    rows = (app$num.edit.seminar.table.handler[prefix]+1):n

  app$num.edit.seminar.table.handler[prefix] = n
  editBtnId = paste0(prefix,"semEditBtn_",rows)
  copyEditBtnId = paste0(prefix,"semCopyEditBtn_",rows)
  studsemBtnId = paste0(prefix,"studsemBtn_",rows)
  reportBtnId = paste0(prefix,"reportBtn_",rows)

  for (i in seq_along(rows)) {
    row = rows[i]
    buttonHandler(editBtnId[i],edit.seminar.click,row=row, prefix=prefix, mode="edit",if.handler.exists = "skip")
    buttonHandler(copyEditBtnId[i],edit.seminar.click, row=row, prefix=prefix, mode="copyedit",if.handler.exists = "skip")
    buttonHandler(studsemBtnId[i],studsem.click, row=row, prefix=prefix)
    buttonHandler(reportBtnId[i],report.click, row=row, prefix=prefix)

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

  studsemBtnId = paste0(prefix,"studsemBtn_",rows)
  studsemBtns = extraSmallButtonVector(id=studsemBtnId, label="students")
  studsemBtns[!df$semester %in% app$glob$semesters.with.matchings] = ""

  reportBtnId = paste0(prefix,"reportBtn_",rows)
  reportBtns = extraSmallButtonVector(id=reportBtnId, label="report")
  reportBtns[!df$semester %in% app$glob$semesters.with.matchings] = ""


  editBtnId = paste0(prefix,"semEditBtn_",rows)
  editBtns = extraSmallButtonVector(id=editBtnId, label="edit")

  copyEditBtnId = paste0(prefix,"semCopyEditBtn_",rows)
  copyEditBtns = extraSmallButtonVector(id=copyEditBtnId, label="new")

  if (prefix=="p") {
    deleteBtnId = paste0(prefix,"semDeleteBtn_",rows)
    deleteBtns = extraSmallButtonVector(id=copyEditBtnId, label="delete")
    deleteBtns[df$locked] = ""
  } else {
    deleteBtns = rep("", NROW(df))
  }

  btns = paste0(studsemBtns,reportBtns,editBtns,copyEditBtns, deleteBtns, sep=" \n")

  cols = setdiff(colnames(df),c("semid", "groupid","locked","active","enabled"))
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
  topics.ui = form.ui.handsone.table(form = glob$semtopicsform,data = top.df, stretchH="last", height="500px")
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
  buttonHandler("exitEditSemBtn", function(...) {
    load.teacher.se(se=se)
    show.edit.sem.main(se=se)
  })

  setUI("semEditUI",ui)
}

save.sem.click = function(se=app$se, app=getApp(),...) {
  restore.point("save.sem.click")
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
  crit.df$semester = se$seminar$semester

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
      top.df$semester = se$seminar$semester
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


studsem.click=function(se = app$se, app=getApp(),prefix, row,...) {
  restore.point("studsem.click")

  if (prefix=="a") {
    seminars = se$aseminars
  } else {
    seminars = se$pseminars
  }

  se$seminar = as.list(seminars[row,])
  se$semstuds = load.studsem(se=se)

  show.studsem.ui(se=se, app=app)
}


load.studsem = function(semid=se$seminar$semid,semtopic=se$semtopic,db=se$db, se=NULL) {
  restore.point("load.semstuds")
  semester=se$seminar$semester
  if (is.null(semid)) return(NULL)

  sql = "
  select * from assign
  NATURAL LEFT JOIN students
  NATURAL LEFT JOIN semtopic
  WHERE (assign.semid = :semid AND
        assign.semester = :semester)

  "

  df = dbGet(db,sql = sql,params = nlist(semester,semid))
  if (NROW(df)>0) {
    df$num = 1:NROW(df)
  } else {
    df$num = integer(0)
  }
  df
}


show.studsem.ui = function(se, app=getApp()) {
  restore.point("show.semstud.ui")

  glob = app$glob
  seminar = se$seminar

  if (NROW(se$semstuds)==0) {
    ui = fluidRow(column(width=10, offset=1,
      p("There are no students yet inscribed in the seminar.")
    ))
    dsetUI("assignUI",ui)
    updateTabsetPanel(session=app$session,inputId = "mainTabset",selected = "assignTab")

    return()
  }


  stud.df = table.form.default.values(glob$semstudform, data=se$semstuds)
  se$org.semstuds.df = stud.df
  #semstud.ui = form.ui.handsone.table(form = glob$semstudform,data = stud.df)
  # Choose columns
  stud.df = stud.df[,setdiff(colnames(stud.df),c("userid"))]

  # Topics
  tdf = dbGet(se$db,"semtopic",params = nlist(semid=se$seminar$semid, semester=se$seminar$semester))


  if (NROW(tdf)>0) {
    tdf = left_join(tdf, select(se$semstuds,topic_ind, name,email),by="topic_ind")

    tdf = tdf[,setdiff(colnames(tdf),c("userid","semid","semester"))]
    topics.ui = tagList(
      h4("Topics"),
      HTML(html.table(tdf))
    )
  } else {
    topics.ui = tagList(
      h4("Topics"),
      p("There are no topics specified for the seminar")
    )
  }


  # Add and delete student ui
  ar.ui = tagList(
    hr(),
    h4("Add or remove student from seminar"),
    textInput("arEmailInput","Student email",value = ""),
    tags$table(tags$tr(      tags$td(
        actionButton("arAddButton","Add student"),
        actionButton("arRemoveButton","Remove student")
      )
    )),
    uiOutput("arInfo"),
    hr()
  )
  setUI("arInfo","")



  ui = fluidRow(column(width=10, offset=1,
    h3(se$seminar$name," - ", se$seminar$teacher," - ", se$seminar$semester),
    h4("Students"),
    HTML(html.table(stud.df)),
    ar.ui,
    topics.ui
  ))

  buttonHandler("arAddButton",add.student.to.seminar)
  buttonHandler("arRemoveButton",remove.student.from.seminar)

  setUI("assignUI",ui)
  #dsetUI("assignUI",ui)
  updateTabsetPanel(session=app$session,inputId = "mainTabset",selected = "assignTab")

}


add.student.to.seminar = function(email = NULL,seminar=se$seminar, semstuds=se$semstuds, app=getApp(),se=app$se,...) {
  if (is.null(email))
    email = getInputValue("arEmailInput")

  restore.point("add.student.to.seminar")

  if (is.null(email) | isTRUE(nchar(email)==0)) {
    msg = colored.html(paste0("You must enter the email adress of the student you want to add to the seminar."), color="red")
    dsetUI("arInfo",HTML(msg))
    return()

  }
  student = dbGet(se$db,"students",params = list(semester=seminar$semester,email=email))
  if (is.null(student)) {
    msg = colored.html(paste0("No student with email ", email, " is registered in semester ", seminar$semester,"."), color="red")
    setUI("arInfo",HTML(msg))
    return()
  }
  userid = student$userid

  if (userid %in% semstuds$userid) {
    msg = colored.html(paste0("The student with email ", email, " is already allocated to the seminar."), color="red")
    setUI("arInfo",HTML(msg))
    return()
  }

  manual = list(
    editid = se$userid,
    semid = seminar$semid,
    userid = student$userid,
    semester = seminar$semester,
    added = TRUE,
    topic_ind = NA_integer_,
    edit_type = "ta",
    edit_time = as.POSIXct(Sys.time())
  )
  assign = list(
    semid = seminar$semid,
    userid = student$userid,
    semester = seminar$semester,
    assign_method = "ta",
    topic_ind = NA_integer_,
    assign_time = as.POSIXct(Sys.time())
  )

  dbBegin(se$db)
  dbInsert(se$db,"manual",manual, schema = app$glob$schemas$manual)
  dbInsert(se$db,"assign",assign, schema = app$glob$schemas$assign)
  dbCommit(se$db)

  # reload form
  se$semstuds = load.studsem(se=se)
  show.studsem.ui(se=se)
}

remove.student.from.seminar = function(email = NULL,seminar=se$seminar, semstuds=se$semstuds, app=getApp(),se=app$se,...) {
  if (is.null(email))
    email = getInputValue("arEmailInput")

  restore.point("remove.student.from.seminar")

  if (is.null(email) | isTRUE(nchar(email)==0)) {
    msg = colored.html(paste0("You must enter the email adress of the student you want to remove from the seminar."), color="red")
    dsetUI("arInfo",HTML(msg))
    return()

  }
  student = dbGet(se$db,"students",params = list(semester=seminar$semester,email=email))
  if (is.null(student)) {
    msg = colored.html(paste0("No student with email ", email, " is registered in semester ", seminar$semester,"."), color="red")
    setUI("arInfo",HTML(msg))
    return()
  }
  userid = student$userid

  if (!userid %in% semstuds$userid) {
    msg = colored.html(paste0("The student with email ", email, " is not allocated to the seminar."), color="red")
    setUI("arInfo",HTML(msg))
    return()
  }

  manual = list(
    editid = se$userid,
    semid = seminar$semid,
    userid = student$userid,
    semester = seminar$semester,
    added = FALSE,
    topic_ind = NA_integer_,
    edit_type = "tr",
    edit_time = as.POSIXct(Sys.time())
  )

  assign = list(
    semid = seminar$semid,
    userid = student$userid,
    semester = seminar$semester,
    assign_method = "ma",
    topic_ind = NA_integer_,
    assign_time = as.POSIXct(Sys.time())
  )

  dbBegin(se$db)
  dbInsert(se$db,"manual",manual, schema = app$glob$schemas$manual)
  dbDelete(se$db,"assign",params = list(userid=student$userid, semester=seminar$semester,semid=seminar$semid))
  dbCommit(se$db)

  # reload form
  se$semstuds = load.studsem(se=se)
  show.studsem.ui(se=se)
}



show.teacher.overview = function(se=app$se, app=getApp()) {
  restore.point("show.teacher.overview")
  envir = c(se$admin, list(today=as.Date(Sys.time())))

  cr = app$glob$rmd.li[["teacher_overview"]]
  header = render.compiled.rmd(cr, envir=envir)
  ui = fluidRow(column(offset=1, width=10,
    HTML(header)
  ))
  setUI("overviewUI", ui)
}


report.click=function(se = app$se, app=getApp(),prefix, row,...) {
  restore.point("report.click")




  if (prefix=="a") {
    seminars = se$aseminars
  } else {
    seminars = se$pseminars
  }

  se$seminar = as.list(seminars[row,])

  round = 1
  env = as.environment(list(semester=se$seminar$semester, semid=se$seminar$semid, semdb=se$db,round=1))
  parent.env(env) = environment()
  rmd = app$glob$reports.rmd[[1]]

  html = try(knit.rmd.in.temp(rmd,envir = env, fragment.only = TRUE, use.commonmark=TRUE))
  if (is(html,"try-error")) {
    html = as.character(html)
  }
  setUI("reportUI",HTML(html))
  updateTabsetPanel(session=app$session,inputId = "mainTabset",selected = "reportTab")

}
