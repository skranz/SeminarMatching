debug.log = function(expr, log.file="debug.txt") {
  conn = file(log.file,"w")
  writeLines(paste0("# Start ", Sys.time()), log.file)

  for (i in seq_along(expr)) {
    call = expr[[i]]
    if (i==1)
      if (isTRUE(as.character(call)=="{"))
        next
    writeLines(deparse(call), conn)
    res = try(eval(call))
    if (is(res,"try-error")) {
      msg = as.character(res)
      msg = gsub(" in eval(call)","",msg, fixed=TRUE)
      writeLines(msg,conn)
      break
    }

  }
  close(conn)
}

expr = quote({
  x = 1
  y = z+3
})

debug.log(quote({
  x = 1
  y = z+3
}))
expr[[3]]
