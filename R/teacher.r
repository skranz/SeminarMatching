# Teacher App for Seminar Matching
# Global Variables
# se main variable contains, state and info on all group seminars
# cs current seminar, detailed info on currently active seminar

examples.EditSeminarApp = function() {
  library(shinyEventsUI)
  library(shinyAce)
  setwd("D:/libraries/SeminarMatching/semapps/shared")
  app = EditSeminarsApp(init.userid = "test", init.password="test", lang="en")
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

  form = load.and.init.form(file =paste0(yaml.dir,"/staffform.yaml"),lang = lang,prefix = "group_")
  form$sets = glob$sets
  glob$staffform = form


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
    se$user = dbGet(se$db,"groupstaff",list(userid=userid),schema = app$glob$schemas[["groupstaff"]])
    se$semester = glob$cur_admin$semester
    se$cur_semester = se$semester
    se$admin = glob$cur_admin


    if (is.null(se$admin)) {
      show.html.warning("mainUI",paste0("Basic administration data is missing that specifies the current semester. The administrator has to add it before seminars can be specified."))
      return()
    }
    # check if user is allowed to edit seminars
    if (NROW(se$user)==0) {
      show.html.warning("mainUI",paste0("The user ", userid, " has not been given any rights to edit seminars in any group."))
      return()
    } else if (sum(se$user$edit_sem)==0) {
      show.html.warning("mainUI",paste0("The user ", userid, " has not been given any rights to edit seminars in any group."))
      return()
    }
    se$groupid = se$user$groupid[1]
    if (isTRUE(se$user$admin)) {
      se$staff = dbGet(se$db,"groupstaff",list(groupid=se$groupid),schema = app$glob$schemas[["groupstaff"]])
    }

    ui = teacher.main.ui()
    setUI("mainUI",ui)
    setUI("activeSemUI", h4("No seminar selected"))
    load.teacher.se(semester=se$semester)
    #radioBtnGroupHandler("mainBtnGroup",function(...){})
    show.teacher.seminars(se=se)
    show.teacher.overview(se=se)
    show.staff.ui(se=se)
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
    show.teacher.seminars()
  })

  appInitHandler(function(session,...) {
    initLoginDispatch(lop)
  })

  jsCode <- "shinyjs.openLink = function(url){window.open(url,'_blank');}"
  app$ui = tagList(
    useShinyjs(),
    extendShinyjs(text = jsCode),
    jqueryLayoutHeader(),
    bootstrapPage(
      uiOutput("mainUI")
    ),
    tags$head(tags$style(HTML('body, html {height: 100%;})')))
  )
  app$lop = lop
  app
}

teacher.main.ui = function(se) {
  restore.point("teacher.main.ui")

  json.opts = '
    defaults: {
      resizable: true,
      closable: false
    },
    west: {
      size: 0.5
    }
  '
  style = tags$style(HTML('
    .ui-layout-pane {
    	padding:	2px;
      padding-left: 10px;
      padding-right: 10px;
    	background:	#FFF;
    	border:		none;
    	overflow:	auto;
    }
    '
  ))

  ui = tagList(
    div(id="mainLayoutDiv", style="height:100vh; overflow: auto",
    jqueryLayoutPanes(id="mainLayout", parent="#mainLayoutDiv",style=style,json.opts=json.opts,
      west = div(
        radioBtnGroup("mainBtnGroup",
          labels=c("Info","Seminars","Group Staff"),
          values = c("infoTab","semTab","semStaff"),
          panes = c("overviewDiv","seminarsDiv","staffDiv")
        ),
        hr(style="margin: 0px; padding: 0px; border-color: grey;"),

        div(id="overviewDiv",uiOutput("overviewUI")),
        div(id="seminarsDiv",style = "visibility: hidden", uiOutput("seminarsUI")),
        div(id="staffDiv",style = "visibility: hidden", uiOutput("staffUI"))
      ),
      center = frozenHeaderPane(parent.layout="mainLayout", parent.pane = "center",
        head = div(
          uiOutput("activeSemUI"),
          div(id="semHeadDiv", style="display: none",
            radioBtnGroup("seminarBtnGroup",
              labels=c("Edit","Participants","Topics","Reports"),
              values=c("editsem","stud","topics","report"),
              panes=list(c("editsemDiv","editsemHeadDiv"),"studDiv","topicsDiv","reportDiv")
            ),
            div(id="editsemHeadDiv",
              hr(style="margin: 1px;"),
              bsButton("saveSemBtn","Save"),
              bsButton("delSemBtn","Delete Seminar"),
              uiOutput("editSemAlert")
            ),
            hr(style="margin: 0px; padding: 0px; border-color: grey;")
          )
        ),
        content = div(id="semContentDiv",
          div(id="editsemDiv",uiOutput("editsemUI")),
          hidden_div(id="studDiv",uiOutput("studUI")),
          hidden_div(id="topicsDiv",uiOutput("topicsUI")),
          hidden_div(id="reportDiv",uiOutput("reportUI"))
        )
      )
    ))
  )

  radioBtnGroupHandler("seminarBtnGroup", function(value,se=app$se,app=getApp(),...) {
     restore.point("seminarBtnGroup.change")
     se$sem.pane = value
  })
  ui
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

show.teacher.seminars = function(userid=se$userid, yaml.dir=app$glob$yaml.dir, db=app$glob$semdb, se=app$se, app=getApp(), semester=se[["semester"]], semid=NA) {
  restore.point("show.teacher.seminars")


  atable = edit.seminar.table(id="atable",se$aseminars, prefix="a", semid=semid)
  ptable = edit.seminar.table(id="ptable",se$pseminars, prefix="p", semid=semid)


  buttonHandler("createSeminarBtn",create.seminar.click)
  buttonHandler("createSeminarFromBtn",create.seminar.from.click)
  ui = tagList(
    h4(paste0("Seminars for group ",se$groupid)),
    selectInput("semMainSemesterInput",label="Semester", choices=app$glob$sets$semesters, selected=se$semester),
    h5(paste0("Activated Seminars for ",se$semester)),
    HTML(atable),
    br(),
    actionButton("createSeminarBtn","New seminar"),
    actionButton("createSeminarFromBtn","New seminar from template"),
    h5(paste0("Unactivated seminars and previous seminars (can be used as templates)")),
    HTML(ptable)
  )


  setUI("seminarsUI", ui)
  dsetUI("seminarsUI", ui)
}

edit.seminar.table = function(id = "seminarTable", df = se$seminars, prefix="a", se=app$se, app=getApp(), semid=NULL) {
  restore.point("edit.seminar.table")

  if (NROW(df)==0) {
    if (prefix == "a") {
      return("<p>... no seminars activated yet ...</p>")
    } else {
      return("<p>... no seminars ...</p>")
    }
  }

  rows = 1:NROW(df)
  cols = setdiff(colnames(df),c("semid", "groupid","locked","active","enabled"))
  wdf = data.frame(df[,cols])

  tdClickHandler(id = id,auto.select = TRUE, remove.sel.row.selector= "#atable tr, #ptable tr", df=df, fun = function(tableId,data,df,...) {
    args = list(...)
    restore.point("mytdClickHandler")
    cat("Table ", tableId, "was clicked in row ", data$row, " and column ", data$col)
    seminar = as.list(df[data$row,])
    set.current.seminar(seminar=seminar)
  })

  sel.row = which(is.true(df$semid == semid))
  html.table(wdf, id=id, bg.color="#ffffff",sel.row = sel.row)

}


# cs contains already the data from the seminars table
load.current.seminar = function(cs=se$cs, se=app$se, app=getApp()) {
  restore.point("load.current.seminar")

  # Load and adapt criteria
  cs$semcrit = dbGet(se$db,"semcrit",list(semid=cs$semid))

  if (NROW(cs$semcrit)<10) {
    df = empty.df.from.schema(app$glob$schemas$semcrit, 10-NROW(cs$semcrit), semid=cs$semid)
    df$semester = cs$semester
    cs$semcrit = rbind(cs$semcrit,df)
  }

  # topics are currently not used
  if (FALSE) {
  # Load and adapt topics
  cs$semtopic = dbGet(se$db,"semtopic",list(semid=cs$semid))
  if (NROW(cs$semtopic)<30) {
    df = empty.df.from.schema(app$glob$schemas$semtopic, 30-NROW(cs$semtopic), semid=cs$semid, semester=cs$semester, size=1, userid=NA_character_)
    cs$semtopic = rbind(cs$semtopic,df)
  }
  cs$semtopic$ind = 1:NROW(cs$semtopic)
  }
  # Load participants
  cs$semstuds = load.semstuds(cs=cs,se=se)
  cs
}

load.semstuds = function(semid=cs$semid,semtopic=cs$semtopic,db=se$db, cs=se$cs, se=app$se, app=getApp()) {
  restore.point("load.semstuds")
  semester=cs$semester
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
    return(NULL)
  }
  df
}

can.seminar.be.deleted = function(cs=se$cs, se = app$se, app=getApp(),...) {
  restore.point("can.seminar.be.deleted")

  cs.sem.num = get.sem.number(cs$semester)
  se.sem.num = get.sem.number(se$semester)
  if (cs.sem.num < se.sem.num) {
    return(list(ok=FALSE,msg="You cannot delete a seminar from previous semesters."))
  } else if (cs.sem.num == se.sem.num) {
    if (isTRUE(Sys.Date() <= se$admin$round1_done_date)) {
      return(list(ok=FALSE,msg="You cannot delete a seminar since the seminar matching has already taken place this semester."))
    }
    if (isTRUE(Sys.Date() <= se$admin$stud_start_date)) {
      return(list(ok=FALSE,msg="You cannot delete a seminar after the seminar choice has already been activated for students. You still can deactivate the seminar, however. Then students cannot put it in their preference list anymore."))
    }
  }


  if (isTRUE(cs$seminar$active)) {
    return(list(ok=FALSE,msg="An activated seminar cannot be deleted. First deactivate and save it. Then delete the seminar."))
  }


  return(list(ok=TRUE, msg=""))
}




delete.seminar.click=function(cs=se$cs, se = app$se, app=getApp(),...) {
  restore.point("delete.seminar.click")
  can.del = can.seminar.be.deleted(cs=cs, se=se)
  if (!can.del$ok) {
    show.field.alert(msg=can.del$msg,id="editSemAlert")
    return()
  }

  semid = cs$semid
  dbBegin(se$db)


  res = try(dbDelete(se$db,"seminars", list(semid=semid)))
  if (is(res,"try-error")) {
    dbRollback(se$db)
    msg = paste0("Error when modifying database:<br> ",as.character(res))
    show.field.alert(msg=msg,id="editSemAlert")
    return()
  }
  res = try(dbDelete(se$db,"semtopic", list(semid=semid)))
  if (is(res,"try-error")) {
    dbRollback(se$db)
    msg = paste0("Error when modifying database:<br> ",as.character(res))
    show.field.alert(msg=msg,id="editSemAlert")
    return()
  }
  res = try(dbDelete(se$db,"semcrit", list(semid=semid)))
  if (is(res,"try-error")) {
    dbRollback(se$db)
    msg = paste0("Error when modifying database:<br> ",as.character(res))
    show.field.alert(msg=msg,id="editSemAlert")
    return()
  }
  dbCommit(se$db)

  show.field.alert(msg="Successfully saved.",id="editSemAlert", color=NULL)

  se$cs = NULL
  set.no.seminar(se=se)
  load.teacher.se(se=se)
  show.teacher.seminars(se=se, semid = NA)
}


create.seminar.click=function(se = app$se, app=getApp(),...) {
  restore.point("create.seminar.click")
  cs = new.env()
  cs$seminar = empty.row.from.schema(app$glob$schemas$seminars, groupid=se$groupid, semester=se$semester, semester=se$semester)

  cs$semcrit = empty.df.from.schema(app$glob$schemas$semcrit, 10)
  cs$semcrit$semester = se$semester


  set.new.seminar(cs=cs,se=se)

}


create.seminar.from.click=function(cs=se$cs, se = app$se, app=getApp(),...) {
  restore.point("create.seminar.from.click")

  cs = as.environment(as.list(cs))
  set.new.seminar(cs=cs,se=se)

}


set.no.seminar = function(se = app$se, app=getApp()) {
  se$cs = NULL
  dsetUI("activeSemUI", h4(paste(cs$semester, "No Seminar Selected")))
  setHtmlHide(id=c("semHeadDiv","editSemHeadDiv","semContentDiv"))
}

set.new.seminar = function(cs, se = app$se, app=getApp()) {
  restore.point("set.new.seminar")
  prev.semid =se$cs[["semid"]]
  cs$semid = cs$seminar$semid = NA
  cs$semester = cs$seminar$semester = cs$semcrit$semester = se$semester
  if (!is.null(cs$semtopic))
    cs$semtopic$semester = cs$semester

  se$cs = cs
  dsetUI("activeSemUI", h4(paste(cs$semester, "New Seminar")))

  show.sem.edit.ui(se=se, app=app)
  setHtmlShow(id="semContentDiv")
  if (is.null(se[["sem.pane"]])) {
    setHtmlShow(id="semHeadDiv")
  }

  setUI("topicsUI",h4("The seminar is not yet created."))
  setUI("studUI",h4("The seminar is not yet created."))
  setUI("reportUI",h4("The seminar is not yet created."))
}

set.current.seminar = function(seminar, se = app$se, app=getApp()) {
  restore.point("set.current.seminar")
  prev.semid =se$cs[["semid"]]
  cs = new.env()
  cs$seminar = seminar
  cs$semid = seminar$semid
  cs$semester = seminar$semester

  cs = load.current.seminar(cs=cs)
  se$cs = cs
  dsetUI("activeSemUI", h4(paste(cs$semester, cs$seminar$semname)))


  show.sem.edit.ui(se=se, app=app)
  setHtmlShow(id="semContentDiv")
  if (is.null(se[["sem.pane"]])) {
    restore.point("jncrb47z4rbfd")

    setHtmlShow(id="semHeadDiv")
  }
  show.sem.stud.ui(se=se,cs=cs, app=app)
  show.sem.topics.ui(se=se,cs=cs, app=app)
  show.sem.report.ui(se=se,cs=cs, app=app)

}

edit.seminar.click=function(cs=se$cs, se = app$se, app=getApp(),mode="edit", prefix="a", row=1,...) {
  restore.point("edit.seminar.click")

  if (prefix=="a") {
    seminars = se$aseminars
  } else {
    seminars = se$pseminars
  }

  cs$seminar = as.list(seminars[row,])

  cs$semcrit = dbGet(se$db,"semcrit",list(semid=cs$semid))

  if (NROW(cs$semcrit)<10) {
    df = empty.df.from.schema(app$glob$schemas$semcrit, 10-NROW(cs$semcrit), semid=cs$semid)
    df$semester = se$semester
    cs$semcrit = rbind(cs$semcrit,df)
  }

  cs$semtopic = dbGet(se$db,"semtopic",list(semid=cs$semid))
  if (NROW(cs$semtopic)<30) {
    df = empty.df.from.schema(app$glob$schemas$semtopic, 30-NROW(cs$semtopic), semid=cs$semid, semester=se$semester, size=1, userid=NA_character_)
    cs$semtopic = rbind(cs$semtopic,df)
  }
  cs$semtopic$ind = 1:NROW(cs$semtopic)



  if (mode=="copyedit") {
    cs$semid = NA_integer_
    cs$active = FALSE
    cs$semester = se$semester
    cs$locked = FALSE

    cs$semcrit$semid = NA_integer_
    cs$semcrit$semester = se$semester

  }

  show.sem.edit.ui(se=se, app=app)
}


show.sem.edit.ui = function(cs = se$cs,se=NULL, app=getApp(), edit=isTRUE(!is.na(cs$semid))) {
  restore.point("show.sem.edit.ui")

  glob = app$glob

  seminar = cs$seminar
  form = glob$semform
  form.vals = form.default.values(glob$semform,values = seminar)
  #cat("show.sem.edit.ui form.vals:\n")
  #print(form.vals)
  form.ui = form.ui.simple(glob$semform, values=form.vals,add.submit = FALSE)


  topics.ui = crit.ui = NULL

  crit.df = table.form.default.values(glob$semcritform, data=cs$semcrit)
  se$org.crit.df = crit.df
  crit.ui = form.ui.handsone.table(form = glob$semcritform,data = crit.df)

  ui = tagList(
    br(),
    form.ui,
    crit.ui
  )

  buttonHandler("saveSemBtn",save.sem.click)
  buttonHandler("delSemBtn",delete.seminar.click)
  clear.field.alert(id="editSemAlert")

  #dsetUI("editsemUI",ui)
  setUI("editsemUI",ui)
  evalJS("Shiny.bindAll();") # need for form to be updated

  #cat("get.form.values:\n")
  #sres = get.form.values(glob$semform)

}

save.sem.click = function(cs=se$cs, se=app$se, app=getApp(),...) {
  restore.point("save.sem.click")
  glob  = app$glob

  sres = get.form.values(glob$semform)

  # We need the NULL value to return original table
  # if there were no changes to the table
  crit.df  = get.table.form.df(glob$semcritform, null.value = se$org.crit.df)

  restore.point("save.sem.click")
  if (!sres$ok) {
    show.field.alert(msg="Could not save, since not all fields are correctly entered.",id="editSemAlert")
    return()
  }

  if (is.null(cs$seminar$enabled))
      cs$seminar$enabled = TRUE
  if (is.na(cs$seminar$enabled))
      cs$seminar$enabled = TRUE

  cs$seminar = copy.intersect(cs$seminar,sres$values)

  dbBegin(se$db)
  new.sem = is.na(cs$semid)
  # insert new seminar

  if (new.sem) {
    res = try(dbInsert(se$db,"seminars",cs$seminar,mode = "insert",schema=glob$schemas$seminars,get.key=TRUE))
  # update existing seminar
  } else {
    res = try(dbInsert(se$db,"seminars",cs$seminar,mode = "replace", schema=glob$schemas$seminars))
  }

  if (is(res,"try-error")) {
    dbRollback(se$db)
    msg = paste0("Error when saving into database:<br> ",as.character(res))
    show.field.alert(msg=msg,id="editSemAlert")
    return()
  }
  cs$seminar = res$values

  cs$semid = semid = cs$seminar$semid

  crit.df$pos = 1:NROW(crit.df)
  crit.df$semid = semid
  crit.df$semester = cs$semester

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

  cs$semcrit = as_data_frame(res$values)

  dbCommit(se$db)

  show.field.alert(msg="Successfully saved.",id="editSemAlert", color=NULL)
  load.teacher.se(se=se)
  show.teacher.seminars(se=se, semid = cs$semid)

  if (new.sem) {
    set.current.seminar(seminar = cs$seminar)
  }

}


studsem.click=function(cs = se$cs, se = app$se, app=getApp(),prefix, row,...) {
  restore.point("studsem.click")

  if (prefix=="a") {
    seminars = se$aseminars
  } else {
    seminars = se$pseminars
  }

  cs$seminar = as.list(seminars[row,])
  cs$semstuds = load.semstuds(se=se)

  show.sem.stud.ui(se=se, app=app)
}

show.sem.topics.ui = function(cs=se$cs, se=app$se, app=getApp()) {
  restore.point("show.sem.topics.ui")


  ui = tagList(
    h4("Topic assignment is not yet implemented.")
  )

  setUI("topicsUI",ui)
  dsetUI("topicsUI",ui)
  return()

  # distinguish whether students are already
  # matched or not
  glob=app$glob
  # Topics
  tdf =cs$semtopic
  tdf = left_join(tdf, select(cs$semstuds,topic_ind, email) %>% filter(!is.na(topic_ind)),by="topic_ind")


  top.df = table.form.default.values(glob$semtopicsform, data=tdf)

  topics.ui = form.ui.handsone.table(form = glob$semtopicsform,data = top.df, stretchH="last", height="500px")
  cs$org.top.df = top.df

  ui = tagList(
    topics.ui
  )

  setUI("topicsUI",ui)
  dsetUI("topicsUI",ui)
}

save.sem.topics =  function(cs=se$cs, se=app$se, app=getApp(),...) {
  glob  = app$glob
  semid = cs$semid

  top.df = get.table.form.df(glob$semtopicsform, null.value = cs$org.top.df)
  restore.point("save.topics")

  dbBegin(se$db)
  # first delete all existing seminar topics
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
      top.df$semester = cs$semester
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


show.sem.stud.ui = function(cs=se$cs, se=app$se, app=getApp()) {
  restore.point("show.semstud.ui")

  glob = app$glob
  if (NROW(cs$semstuds)==0) {
    ui = tagList(
      p("There are no students yet inscribed in the seminar.")
    )
    dsetUI("studUI",ui)
    return()
  }

  stud.df = table.form.default.values(glob$semstudform, data=cs$semstuds)
  cs$org.semstuds.df = stud.df
  #semstud.ui = form.ui.handsone.table(form = glob$semstudform,data = stud.df)
  # Choose columns
  stud.df = stud.df[,setdiff(colnames(stud.df),c("userid"))]

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

  emails.string = paste0(stud.df$email, collapse = ", ")

  ui = tagList(
    h4("Participants"),
    HTML(html.table(stud.df)),
    h4("Participants Emails"),
    shinyAce::aceEditor("studEmailListAce", value=emails.string, wordWrap = TRUE, height="4em"),
    ar.ui
  )

  buttonHandler("arAddButton",add.student.to.seminar)
  buttonHandler("arRemoveButton",remove.student.from.seminar)

  setUI("studUI",ui)
  dsetUI("studUI",ui)
}


add.student.to.seminar = function(email = NULL,seminar=cs$seminar, semstuds=cs$semstuds, app=getApp(),se=app$se, cs=se$cs,...) {
  if (is.null(email))
    email = getInputValue("arEmailInput")

  restore.point("add.student.to.seminar")

  if (is.null(email) | isTRUE(nchar(email)==0)) {
    msg = colored.html(paste0("You must enter the email adress of the student you want to add to the seminar."), color="red")
    dsetUI("arInfo",HTML(msg))
    return()

  }
  student = dbGet(se$db,"students",params = list(semester=cs$semester,email=email))
  if (is.null(student)) {
    msg = colored.html(paste0("No student with email ", email, " is registered in semester ", cs$semester,"."), color="red")
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
    semid = cs$semid,
    userid = student$userid,
    semester = cs$semester,
    added = TRUE,
    topic_ind = NA_integer_,
    edit_type = "ta",
    edit_time = as.POSIXct(Sys.time())
  )
  assign = list(
    semid = cs$semid,
    userid = student$userid,
    semester = cs$semester,
    assign_method = "ta",
    topic_ind = NA_integer_,
    assign_time = as.POSIXct(Sys.time())
  )

  dbBegin(se$db)
  dbInsert(se$db,"manual",manual, schema = app$glob$schemas$manual)
  dbInsert(se$db,"assign",assign, schema = app$glob$schemas$assign)
  dbCommit(se$db)

  # reload form
  cs$semstuds = load.semstuds(cs=cs)
  show.sem.stud.ui(se=se)
}

remove.student.from.seminar = function(email = NULL,seminar=cs$seminar, semstuds=cs$semstuds, app=getApp(),se=app$se,cs=se$cs,...) {
  if (is.null(email))
    email = getInputValue("arEmailInput")

  restore.point("remove.student.from.seminar")

  if (is.null(email) | isTRUE(nchar(email)==0)) {
    msg = colored.html(paste0("You must enter the email adress of the student you want to remove from the seminar."), color="red")
    dsetUI("arInfo",HTML(msg))
    return()

  }
  student = dbGet(se$db,"students",params = list(semester=cs$semester,email=email))
  if (is.null(student)) {
    msg = colored.html(paste0("No student with email ", email, " is registered in semester ", cs$semester,"."), color="red")
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
    semid = cs$semid,
    userid = student$userid,
    semester = cs$semester,
    added = FALSE,
    topic_ind = NA_integer_,
    edit_type = "tr",
    edit_time = as.POSIXct(Sys.time())
  )

  assign = list(
    semid = cs$semid,
    userid = student$userid,
    semester = cs$semester,
    assign_method = "ma",
    topic_ind = NA_integer_,
    assign_time = as.POSIXct(Sys.time())
  )

  dbBegin(se$db)
  dbInsert(se$db,"manual",manual, schema = app$glob$schemas$manual)
  dbDelete(se$db,"assign",params = list(userid=student$userid, semester=cs$semester,semid=cs$semid))
  dbCommit(se$db)

  # reload form
  cs$semstuds = load.semstuds(cs=cs)
  show.sem.stud.ui(se=se)
}



show.teacher.overview = function(se=app$se, app=getApp()) {
  restore.point("show.teacher.overview")
  envir = c(se$admin, list(today=as.Date(Sys.time())))

  cr = app$glob$rmd.li[["teacher_overview"]]
  .GlobalEnv$knit_print.Date = function(x,...) {format(x, format="%a. %d.%m.%Y")}

  header = render.compiled.rmd(cr, envir=envir, use.print="knit")
  ui = tagList(
    HTML(header)
  )
  setUI("overviewUI", ui)
}


show.sem.report.ui =function(cs = se$cs,se = app$se, app=getApp()) {
  restore.point("show.sem.report.ui")

  round = 1
  env = as.environment(list(semester=cs$semester, semid=cs$semid, semdb=se$db,round=1))
  parent.env(env) = environment()
  rmd = app$glob$reports.rmd[[1]]

  html = try(knit.rmd.in.temp(rmd,envir = env, fragment.only = TRUE, use.commonmark=TRUE))
  if (is(html,"try-error")) {

    html = as.character(html)
    html = p("No data on studet preferences for this seminar available.")
  }
  dsetUI("reportUI",HTML(html))
  setUI("reportUI",HTML(html))
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


show.staff.ui = function(se=app$se, app=getApp(), sel.row=NULL) {
  restore.point("show.staff.ui")

  glob = app$glob
  if (!isTRUE(se$user$admin)) {
    setUI("staffUI",h4(paste0("You have no permission to change the staff of group ", se$groupid)))
    return()
  }
  form = glob$staffform
  form.vals = form.default.values(glob$staffform,values = se$sel.staff)
  form.ui = form.ui.simple(glob$staffform, values=form.vals,add.submit = FALSE)

  df = se$staff

  color = ifelse(df$boss,"#ccccff", "#ffffff")

  staff.table = html.table(se$staff, id="staffTable", bg.color=color,sel.row = sel.row)

  tdClickHandler(id = "staffTable",auto.select = TRUE,df=se$staff, fun = function(tableId,data,...,se=app$se, app=getApp()) {
    args = list(...)
    restore.point("staffTableClick")
    cat("Table ", tableId, "was clicked in row ", data$row, " and column ", data$col)
    se$sel.staff = as.list(df[data$row,])
    show.staff.ui(se=se, sel.row=data$row)
    #set.current.seminar(seminar=seminar)
  })


  ui = tagList(
    h4("Staff that is allowed to change seminars"),
    HTML(staff.table),
    actionButton("addStaffBtn","Add to Staff"),
    actionButton("changeStaffBtn","Change Permissions"),
    actionButton("delStaffBtn","Remove from Staff"),
    uiOutput("staffAlert"),
    br(),
    form.ui
  )

  buttonHandler("addStaffBtn",add.staff.click)
  buttonHandler("delStaffBtn",delete.staff.click)
  buttonHandler("changeStaffBtn",change.staff.click)

  clear.field.alert(id="staffAlert")

  setUI("staffUI",ui)
  evalJS("Shiny.bindAll();") # need for form to be updated

}



add.staff.click = function(...,se=app$se,app=getApp()) {

  sres = get.form.values(app$glob$staffform)
  restore.point("add.staff.click")
  if (!sres$ok) {
    show.field.alert(msg="Not all fields are correctly entered.",id="staffAlert")
    return()
  }
  vals = sres$values
  all = dbGet(se$db,"groupstaff")
  if (vals$email %in% all$email) {
    group.id = all$groupid[which(all$email==values$email)[1]]
    show.field.alert(msg=paste0("The user with email ", vals$email, " is already member of the group ", group.id,". He must be first deleted before he can be added to this group."),id="staffAlert")
    return()
  }
  vals$userid = vals$email
  vals$groupid = se$groupid
  vals$boss = FALSE

  res = dbInsert(se$db,"groupstaff",vals = vals,schema = app$glob$schemas$groupstaff)
  restore.point("add.staff.click2")

  se$staff = rbind(se$staff, res$values)
  se$sel.staff = NULL
  show.staff.ui()
}

change.staff.click = function(...,se=app$se,app=getApp()) {

  sres = get.form.values(app$glob$staffform)
  restore.point("change.staff.click")
  if (!sres$ok) {
    show.field.alert(msg="Not all fields are correctly entered.",id="staffAlert")
    return()
  }
  vals = sres$values
  all = se$staff
  row = match(vals$email, all$email)
  if (is.na(row)) {
    show.field.alert(msg=paste0("The user with email ", vals$email, " is not yet member of the group ", group.id,". Click the add button instead."),id="staffAlert")
    return()
  }
  if (isTRUE(all$boss[row])) {
    show.field.alert(msg=paste0("You cannot change permissions for that user."),id="staffAlert")
    return()
  }
  vals$userid = vals$email
  vals$groupid = se$groupid
  vals$boss = all$boss[row]

  res = dbInsert(se$db,"groupstaff",vals = vals,schema = app$glob$schemas$groupstaff,mode = "replace")
  restore.point("change.staff.click2")

  se$staff[row,] =res$values
  se$sel.staff = NULL
  show.staff.ui()
}

delete.staff.click = function(...,se=app$se,app=getApp()) {

  sres = get.form.values(app$glob$staffform)
  restore.point("change.staff.click")
  if (!sres$ok) {
    show.field.alert(msg="Not all fields are correctly entered.",id="staffAlert")
    return()
  }
  vals = sres$values
  all = se$staff
  row = match(vals$email, all$email)
  if (is.na(row)) {
    show.field.alert(msg=paste0("The user with email ", vals$email, " is no member of the group ", group.id),id="staffAlert")
    return()
  }
  if (isTRUE(all$boss[row])) {
    show.field.alert(msg=paste0("That user cannot be deleted from the group."),id="staffAlert")
    return()
  }

  vals$userid = vals$email
  vals$groupid = se$groupid
  vals$boss = all$boss[row]

  res = dbDelete(se$db,"groupstaff",params=list(userid=vals$userid))
  restore.point("change.staff.click2")

  se$staff = se$staff[-row,,drop=FALSE]
  se$sel.staff = NULL
  show.staff.ui()

}
