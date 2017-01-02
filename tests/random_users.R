# create random users in login database

create.random.users = function(db,n=100, ind=1:n, mode="insert") {
  for (i in ind) {
    userid = paste0("_test_",i)
    email = userid

    password = "test"
    salt = make.salt()
    hash = make.password.hash(password = password, salt=salt)$hash
    user = list(userid=userid, email=email, salt=salt, hash=hash, confirmed=TRUE, create_time=Sys.time())

    try(dbInsert(db, "users",user, mode=mode))
    #try(dbInsert(db, "users",user, mode="insert"))
  }
}


library(RSQLite)
library(SeminarMatching)
setwd("D:/libraries/SeminarMatching/testapps")
app.dir = "D:/libraries/SeminarMatching/testapps"
dbname = file.path(app.dir,"shared","db","loginDB.sqlite")
db=dbConnect(SQLite(), dbname)

create.random.users(db=db,n=10)

dbDisconnect(db)
