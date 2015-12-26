
html.table = function(df, sel.row=NULL, show.header=TRUE, header=colnames(df), row.names=FALSE, border=TRUE, bg.color =c("#dddddd","#ffffff"), sel.color='#ffdc98', font.size="100%", round.digits=8, signif.digits=8,col.tooltips=NULL, ...) {
  restore.point("html.table")
  n = NROW(df)

  #bg.color =c("#ededfe","#fcfcff")
  #F7F7F7
  row.bgcolor = rep(bg.color,length=n)

  if (!is.null(sel.row)) {
    #row.bgcolor[sel.row]='#ffdc98'
    #row.bgcolor[sel.row]='#00ff00'
    row.bgcolor[sel.row]=sel.color
  }

  if (show.header) {
    colnames=header
    if (is.null(col.tooltips)) {
      inner = colnames
    } else {
      inner = paste0('<span title="', col.tooltips,'">', colnames, '<span>')
      #inner[nchar(col.tooltips)==0] = colnames
    }

    head = paste0('<th class="data-frame-th">',inner,'</th>', collapse="")
    head = paste0('<tr>', head, '</tr>')
  } else {
    head = ""
  }


  td.class = rep("data-frame-td", NROW(df))
  if (length(td.class)>0) {
    td.class[length(td.class)]="data-frame-td-bottom"
  }

  cols = 1:NCOL(df)
  code = paste0('"<td class=\\"",td.class,"\\" nowrap bgcolor=\\"",row.bgcolor,"\\">", (df[[',cols,']]),"</td>"', collapse=",")
  code = paste0('paste0("<tr>",',code,',"</tr>", collapse="\\n")')
  call = parse(text=code)
  main = eval(parse(text=code))

  tab = paste0('<table class="data-frame-table">\n', head, main, "\n</table>")

  th.style='font-weight: bold; margin: 5px; padding: 5px; border: solid 1px black; text-align: center;'
  td.style='font-family: Verdana,Geneva,sans-serif; margin: 2px 4px 2px 4px; padding: 3px 5px 3px 5px; border: solid 1px black; text-align: left;'

  if (!is.null(font.size)) {
    th.style = paste0(th.style, "font-size: ", font.size,";")
    td.style = paste0(td.style, "font-size: ", font.size,";")

  }

  tab = paste0("<style>",
    " table.data-frame-table {	border-collapse: collapse;  display: block; overflow-x: auto;}\n",
    " td.data-frame-td {", td.style,"}\n",
    " td.data-frame-td-bottom {", td.style," border-bottom: solid 1px black;}\n",
    " th.data-frame-th {", th.style,"}\n",
    " tbody>tr:last-child>td {
      border-bottom: solid 1px black;
    }\n",
    "</style>",tab
  )

  #writeLines(tab, "test.html")
  tab

  return(tab)
}


