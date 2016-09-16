check.studsem = function(student, sems, se=NULL, ..., lang="en") {

  # warning if seminar is not for same level than student

  incomp = if (student$studBAMA == "Bachelor") "Master" else "Bachelor"

  rows = which(sems$semBAMA == incomp)

  if (length(rows)>0) {
    sem.txt = paste0("'",sems$semname[rows],"'", collapse=", ")
    if (student$studBAMA =="Bachelor") {
      if (lang == "de") {
        msg = paste0("Ihre Praeferenzliste wurde gespeichert.<br>Warnung: Sie haben in Ihren Daten angegeben, dass Sie auf Bachelor Niveau studieren, sie haben aber auch Seminare ausgewaehlt, die nur fuer Master Studierende sind. Dies macht nur Sinn, wenn Sie sich die Seminare bereits fuer den Master anrechnen wollen.")
      } else {
        msg = paste0("Your seminar preferences have been saved.<br>Warning: In your data, you stated that you are a bachelor student, but you also selected some seminars, which are for Master students only. This would only make sense if you already want to hear some seminars for a Master that you plan to continue after the Bachelor.")
      }
      return(list(ok=TRUE, msg=msg))
    } else {
      if (lang == "de") {
        msg = paste0("Ihre Praeferenzliste wurde NICHT gespeichert.<br>Fehler: Sie haben in Ihren Daten angegeben, dass Sie auf Master Niveau studieren, sie haben aber auch Seminare ausgewaehlt die nur fuer Bachelor Studierende sind. Falls Sie das Bachelorstudium noch nicht abgeschlossen haben, und noch Seminare brauchen, waehlen Sie 'Bachelor' als Studiengang. Sie koennen sich trotzedem bereits auf Master Seminare bewerben.")
      } else {
        msg = paste0("Your seminar preferences have NOT been saved.<br>Error: In your data, you stated that you are a Master student, but you also selected some seminars that are for Bachelor students only. Please remove those seminars. If you still are a Bachelor student, change your personal data.")      }
      return(list(ok=FALSE, msg=msg))
    }
  }

  if (lang=="de") {
    msg = "Ihre Seminarpraeferenzen wurden erfolgreich gespeichert."
  } else {
    msg = "Your seminar preferences have been successfully saved."
  }
  return(list(ok=TRUE, msg=msg))
}
