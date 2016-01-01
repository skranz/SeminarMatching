
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

  ui = form.ui.multi.handsome(form=form, data=data)

  add.form.handlers(form)
  app$ui = fluidPage(ui)
  runEventsApp(app, launch.browser = rstudio::viewer)

}


form.ui.multi.handsome = function(id="multiHandsomeUI", form, data, fields=form$fields, submitBtn=NULL, submitLabel="Submit",add.submit=TRUE,lang=form[["lang"]], addLabel="",addIcon=icon(name = icon("plus",lib = "glyphicon")), inner.fun = form.ui.multi.simple.inner, width=first.none.null(form$width,"100%"), height=first.none.null(form$height), stretchH='all', ...) {
  restore.point("form.ui.multi")

  library(rhandsontable)

  cols = names(fields)
  df = data[,cols]

  ui = rHandsontableOutput(outputId = id, width=width, height=height)

  labels = sapply(names(fields),function(name) {
    first.none.null(get.lang.field(fields[[name]],lang)$label, name)
  })
  names(labels) = NULL

  help = sapply(fields,function(field) {
    if (is.null(field$help)) return(NA)
    field$help
  })
  help = matrix(help, nrow=NROW(df), ncol=ncol(df), byrow=TRUE)

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

  ui
}

