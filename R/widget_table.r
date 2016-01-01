
makeVectorInputCode = function(args, fun) {
  fun = checkboxInput
  args = formals(fun)

  ph = as.list(paste0("{{{",names(args),"}}}"))
  names(ph) = names(args)

  ph$value=!TRUE

  str = as.character(do.call(fun, ph))
  str = gsub("{{{","',",str,fixed=TRUE)
  str = gsub("}}}",",'",str,fixed=TRUE)
  #cat(str)

  code = paste0("xxxInputVector = ", deparse(args(fun))[1]," {
  code = paste0('\n", str,"
  )'
  code
}"
  )
  cat(code)
}

make.widget.cols = function(df, cols=colnames(df), inputs=NULL, id.prefix="table_input") {
  restore.point("make.widget.cols")
  n = NROW(df)
  if (is.null(inputs)) {
    inputs = sapply(cols, function(col) {
      if (is.logical(df[[col]])) return("checkbox")
      if (is.numeric(df[[col]])) return("numeric")
      return("text")
    })
  }
  wdf = lapply(seq_along(cols), function(i) {
    col = cols[[i]]
    id =paste0(id.prefix,"__",col,"__",1:n)
    value = df[[col]]
    if (inputs[[i]]=="text")
      return(textInputVector(id,label = "",value = value))
    if (inputs[[i]]=="checkbox")
      return(checkboxInputVector(id,label = "",value = value))
    if (inputs[[i]]=="numeric")
      return(numericInputVector(id,label = "",value = value))
  })
  names(wdf) = cols
  df[cols] = wdf[cols]
  df
}

examples.widget.table = function() {
  df = data.frame(
    x = sample(1:10,10),
    y = sample(1:10,10)
  )
  wdf = make.widget.cols(df,cols = "y",id.prefix = "wdf")

  app = eventsApp()
  app$ui = fluidPage(
    h3("A widget table"),
    DT::dataTableOutput('x1')
  )
  setWidgetTable('x1',wdf)
  runEventsApp(app, launch.browser=rstudio::viewer)
  #runEventsApp(app)
}

setWidgetTable = function(id, df,..., escape=FALSE,options = list(), app=getApp()) {
  restore.point("setWidgetTable")
  options = c(options, list(
     preDrawCallback = JS('function() {
 Shiny.unbindAll(this.api().table().node()); }'),
     drawCallback = JS('function() {
 Shiny.bindAll(this.api().table().node()); } ')
   ))
  setDataTable(id, value=df,escape=escape, options=options,...)
}
