examples.rankingPart = function() {

  library(shiny)
  library(shinyBS)
  library(shinyEvents)
  library(DT)
  library(hwriter)
  library(yaml)
  library(YamlObjects)

  restore.point.options(display.restore.point = TRUE)

  setwd("D:/libraries/SeminarMatching")

  app = eventsApp()


  opts = read.yaml(file="settings.yaml",keep.quotes = FALSE)
  lang = opts$start_lang

  app$opts = opts
  app$lang = lang

  li = read.yaml(file="seminars.yaml", keep.quotes = FALSE)

  cols = union(opts$selSemCols,opts$allSemCols)

  thin.li = lapply(li, function(el) {el[cols]})
  sem.df = rbindlist(thin.li, fill=TRUE)
  sem.df$selected = FALSE
  sem.df$row = 1:NROW(sem.df)
  sem.df$pos = NA

  #app$ui = fluidPage(dataTableOutput("selTable"))
  app$ui = fluidPage(
    HTML(opts$rankingTitle[[lang]]),
    p(opts$rankingDescr[[lang]]),
    bsCollapse(bsCollapsePanel(
      title="Details",
      HTML(opts$rankingBackground[[lang]])
    )),
    h3(opts$selSemTitle[[lang]]),
    uiOutput("selSemUI"),
    h3(opts$allSemTitle[[lang]]),
    uiOutput("allSemUI")
  )

  sel.rows = sample(1:NROW(sem.df),2)
  sel.df = sem.df[sel.rows,]
  sel.df$pos = 1:NROW(sel.df)
  update.selTable(sel.df)
  update.semTable(sem.df, sel.rows=sel.rows)

  add.seminar.choice.handlers(num.sems=NROW(sem.df))

  bsButton("id","label",size="extra-small")

  runEventsApp(app,launch.browser = rstudio::viewer)
}

update.selTable = function(sel.df, sel.row=NULL, app=getApp()) {
  restore.point("update.selTable")

  app$sel.df = sel.df
  if (NROW(sel.df)==0) {
    setUI("selSemUI",p("---"))
    return()
  }

  widget.df = sel.widgets.df(sel.df)
  html = hwrite.selTable(widget.df,sel.row=sel.row)
  setUI("selSemUI",HTML(html))

}


hwrite.selTable = function(widget.df, sel.row=1, app=getApp(), opts=app$opts, lang=app$lang) {
  restore.point("hwrite.selTable")
  html.table(widget.df,sel.row = sel.row, header=opts$selSemColnames[[lang]], bg.color="#ffffff")
}

sel.widgets.df = function(df, cols=app$opts$selSemCols, app=getApp()) {
  restore.point("sel.widgets.df")

  rows = 1:NROW(df)
  upBtnId = paste0("upBtn_",rows)
  downBtnId = paste0("downBtn_",rows)
  removeBtnId = paste0("removeBtn_",rows)

  upBtns = extraSmallButtonVector(id=upBtnId,label="",icon=icon("arrow-up",lib = "glyphicon"))
  downBtns = extraSmallButtonVector(id=downBtnId, label="",icon=icon("arrow-down",lib="glyphicon"))
  removeBtns = extraSmallButtonVector(id=removeBtnId, label="",icon=icon("remove",lib = "glyphicon"))

  btns = paste0(upBtns,downBtns,removeBtns)
  data.frame(Rank=rows,btns,df[,cols])
}


update.semTable = function(sem.df, sel.rows=which(sem.df$selected), app=getApp()) {
  restore.point("update.semTable")

  sem.df$selected[sel.rows] = TRUE

  app$sem.df = sem.df

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


update.selTable.DT = function(sel.df, app=getApp()) {
  restore.point("update.selTable")

  app$sel.df = sel.df
  table = sel.widgets.df(sel.df)
  setWidgetTable('selTable',table,colnames=rep("", NCOL(table)), options=list(searching =FALSE,  ordering = FALSE, paging = FALSE, info=FALSE, serverSide=FALSE))

}


add.seminar.choice.handlers = function(num.sems) {
  rows = 1:num.sems

  upBtnId = paste0("upBtn_",rows)
  downBtnId = paste0("downBtn_",rows)
  removeBtnId = paste0("removeBtn_",rows)
  addBtnId = paste0("addBtn_",rows)

  for (row in rows) {
    buttonHandler(upBtnId[row],updown.click, row=row,up=TRUE)
    buttonHandler(downBtnId[row],updown.click, row=row,up=FALSE)
    buttonHandler(addBtnId[row],add.seminar.click, row=row)
    buttonHandler(removeBtnId[row],remove.seminar.click, pos=row)
  }

}


updown.click = function(app,value,row,up=TRUE,...) {
  sel.df = app$sel.df
  restore.point("updown.click")
  cat("updown.click")

  new.pos = row + 1.5 - 3*up
  sel.df$pos[row] = new.pos
  sel.df = sel.df[order(sel.df$pos),]
  new.row = which(sel.df$pos==new.pos)
  sel.df$pos = rank(sel.df$pos)
  update.selTable(sel.df, sel.row=new.row)

  #table = seminar.table(df=app$rdf)
  #setUI("mainUI", HTML(table))
}

add.seminar.click = function(row, app,...) {
  restore.point("add.seminar.click")

  sem.df = app$sem.df
  sel.df = app$sel.df
  # Seminar does already exist
  if (row %in% sel.df$row) return()
  #
  sel.df = rbind(sel.df,sem.df[row,])
  sel.df$pos = 1:NROW(sel.df)
  sem.df$selected[row] = TRUE
  update.selTable(sel.df,sel.row = NROW(sel.df))
  update.semTable(sem.df)

}

remove.seminar.click = function(pos,app,...) {
  restore.point("remove.seminar.click")

  sem.df = app$sem.df
  sel.df = app$sel.df
  #
  row = sel.df$row[pos]
  sel.df = sel.df[-pos,]
  sem.df$selected[row] = FALSE
  update.selTable(sel.df,sel.row = NULL)
  update.semTable(sem.df)
}
