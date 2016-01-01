replace.whiskers = function(str, env=parent.frame()) {
  restore.point("replace.whiskers")

  pos = str.blocks.pos(str,"{{","}}")
  if (NROW(pos$outer)==0) return(str)
  s = substring(str, pos$inner[,1],pos$inner[,2])
  vals = lapply(s, function(su) {
    res = try(eval(parse(text=su),env))
    if (is(res,"try-error")) res = "`Error`"
    res
  })
  res = str.replace.at.pos(str, pos$outer, unlist(vals))
  res
}


compile.markdown = function(txt, values = NULL, digits=8) {
  restore.point("compile.story.txt")
  if (length(txt)==0) return("")
  if (length(values)>0) {
    val = lapply(values, function(v) {
      if (is.numeric(v)) return(signif(v,digits))
      return(v)
    })
    txt = replace.whiskers(paste0(txt, collapse="\n"), val)
  }
  html = markdownToHTML(text=txt,encoding = "UTF-8", fragment.only=TRUE)
  html
}
