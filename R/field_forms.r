examples.field.ui = function() {

  setwd("D:/libraries/SeminarMatching")

  fields.li = read.yaml(file="fields.yaml",keep.quotes = FALSE)

  ui = make.field.ui(fields.li[[2]])

  app = eventsApp()
  app$ui = fluidPage(ui)
  runEventsApp(app, launch.browser = rstudio::viewer)

}


make.field.ui = function(fields, lang="de") {

  lapply(names(fields), function(name) {
    get.field.widget(field=fields[[name]],name=name,lang=lang)
  })


}

get.field.widget = function(field,name=field$name, lang="de") {
  restore.point("get.field.widget")

  id = paste0(name,"__FieldInput")
  lang.field = field[[paste0("lang_",lang)]]
  if (is.null(field[["choices"]])) {
    inp = textInput(id, label=lang.field$label, value="")
  } else {
    li = as.list(field$choices)
      restore.point("get.field.widget.selectize")
    inp = selectizeInput(id, lang.field$label,choices=li)
  }
  if (!is.null(lang.field$help)) {
    help = helpText(lang.field$help)
  } else {
    help = NULL
  }
  list(inp,help,hr())
}
