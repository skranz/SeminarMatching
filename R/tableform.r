
examples.vecForm = function() {
  setwd("D:/libraries/SeminarMatching/")
  library(yaml)
  library(YamlObjects)


  restore.point.options(display.restore.point = TRUE)
  app = eventsApp()

  sets =read.yaml(file="sets.yaml")
  yaml = paste0(readLines("sempointsform.yaml"), collapse="\n")


  form = read.yaml(text=yaml,utf8 = TRUE)
  form$success.handler = function(...) {
    cat("\nGreat you inserted valid numbers!")
  }
  form$lang = "de"
  form$sets = sets

  set.form(form)
  data = data.frame(
    points = c(10,5),
    slots = c('3',''),
    subject = c('',''),
    spec = c('',''),
    email = c('','')
  )

  ui = form.ui.handsone.table(form=form, data=data)

  add.form.handlers(form)
  app$ui = fluidPage(ui)
  runEventsApp(app, launch.browser = rstudio::viewer)

}


form.ui.handsone.table = function(id="handsoneTableFormUI", form, data, fields=form$fields, label=first.none.null(lang.form[["label"]],form[["label"]]), help_html=lang.form[["help_html"]],note_html=lang.form[["note_html"]],note_title=first.none.null(lang.form[["note_title"]],"Info"), sets = form[["sets"]],
  submitBtn=NULL, submitLabel="Submit",add.submit=TRUE,lang=form[["lang"]], addLabel="",addIcon=icon(name = icon("plus",lib = "glyphicon")), width=first.none.null(form$width,"100%"), height=first.none.null(form$height), stretchH='all', lang.form = get.lang.form(form, lang), ...) {
  restore.point("form.ui.handsone.table")

  library(rhandsontable)

  cols = names(fields)
  df = data[,cols]

  ui = rHandsontableOutput(outputId = id, width=width, height=height)

  labels = sapply(names(fields),function(name) {
    first.none.null(get.lang.field(fields[[name]],lang)$label, name)
  })
  names(labels) = NULL

  #fhelp = sapply(fields,function(field) {
  #  if (is.null(field$help)) return(NA)
  #  field$help
  #})
  #fhelp = matrix(fhelp, nrow=NROW(df), ncol=ncol(df), byrow=TRUE)

  hot = rhandsontable(data=df, colHeaders=labels, rowHeaders=NULL, useTypes = TRUE, readOnly = FALSE, selectCallback = FALSE,stretchH=stretchH)

  for (col in seq_along(fields)) {
    field = fields[[col]]
    input = field[["input"]]
    choices = field[["choices"]]
    choice_set = field[["choice_set"]]

    if (is.null(input)) {
      if (is.null(choices) & is.null(choice_set)) {
        input = "text"
      } else {
        input = "selectize"
      }
    }
    type = NULL
    if (input == "selectize") {
      if (!is.null(choice_set)) {
        for (set in sets[choice_set])
          choices = c(choices, set)
      }
      choices = unlist(choices)
      hot = hot_col(hot,col=col,type="dropdown", source=choices)
    } else {
      hot = hot_col(hot,col=col,type="text", strict=FALSE)
    }
  }

  setRHandsontable(id, hot)

  alert_id = paste0(id,"__Alert")
  ui = list(ui,uiOutput(alert_id))
  if (!is.null(label)) {
    ui = c(list(h4(label)),ui)
  }
  if (!is.null(help_html)) {
    ui = c(ui,list(HTML(help_html)))
  }
  if (!is.null(note_html)) {
    ui =c(ui, list(bsCollapse(bsCollapsePanel(title=note_title,HTML(note_html)))))
  }
  ui
}


table.form.default.values = function(form, data = NULL, nrow=max(NROW(data),1), sets=NULL, boolean.correction=TRUE) {
  restore.point("table.form.default.values")

  df = data
  vals = lapply(form$fields, function(field) {
    rep(field.default.values(form=form, field=field,sets=sets),nrow)
  })
  replace = intersect(names(vals), names(df))

  if (length(replace)>0) {
    if (boolean.correction)
      boolean = sapply(vals[replace], function(val) any(is.logical(val) & !is.na(val)))
    vals[replace] = df[replace]
    if (boolean.correction)
      vals[replace][boolean] =  lapply(vals[replace][boolean], as.logical)
  }
  as.data.frame(vals)
}
