# Function will be called at night each day and performs
# automatic actions like running a seminar matchings if it is due

get.semdb = function(db.dir = paste0(main.dir,"/db"), main.dir = getwd())  {
  restore.point("get.semdb")
  dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())
}

get.current.admin = function(semester=get.default.semester(db=semdb, schemas=schemas), admin=NULL,  schemas = load.and.init.schemas(paste0(schema.dir, "/semdb.yaml")), schema.dir = paste0(main.dir,"/schema"), semdb = get.semdb(main.dir=main.dir), main.dir=getwd()) {

  restore.point("get.current.admin")
  if (is.null(semester)) return(NULL)
  admin = dbGet(semdb,"admin", nlist(semester),schema = schemas$admin)
  if (is.null(admin)) return(NULL)
  admin = as.list(admin)
  init.se.admin(admin)

}

find.auto.tasks = function(admin=NULL,  schemas = load.and.init.schemas(paste0(schema.dir, "/semdb.yaml")), schema.dir = paste0(getwd(),"/schema")) {

  restore.point("find.auto.tasks")
  setwd("D:/libraries/SeminarMatching/semapps/shared")
  if (is.null(admin))
    admin = get.current.admin()


}
