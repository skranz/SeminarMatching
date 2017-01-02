library(SeminarMatching)
setwd("/srv/shiny-server/semapps/shared")
log.file=paste0(getwd(),"/log/auto_run.log")
SeminarMatching::append.log(c("autorun","******"), log.file)
run.auto.tasks()
cat("autorun was called")

#main.dir = getwd()
#admin = get.current.admin(main.dir = main.dir)
#tasks = find.auto.tasks(admin = admin, main.dir = main.dir)

# Run as batch
# Rscript /srv/shiny-server/semapps/shared/autorun_matching.R
# R CMD BATCH R CMD /srv/shiny-server/semapps/shared/autorun_matching.R

# Reset matching
#delete.seminar.matching(semester="SS17")