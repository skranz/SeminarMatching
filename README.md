# SeminarMatching

Author: Sebastian Kranz, Ulm University

## Overview

This is an shiny based R package to perform centralized matching of students to seminars based on a student optimal Gale-Shapley algorithm.

The software is developed and used for a centralized seminar assignment at the economics department of Ulm University. Yet, it is designed such that it can be customized also for other universities or departments. The documentation for customization is fairly poor, however.

- The software consists of several web apps:

    + for lecturers to enter and specify seminars and see results
  
    + for students to enter background data, preferences over seminars and see the results after the matching
  
    + for admins to specify general parameters like the timeline including the date of the centralized matching and to see general reports.

+ Students will be matched via the Gale-Shapley student optimal mechanismn.
+ There is a random component to students' priorities, but lecturers have also the option to give priority over students, e.g. based on already heard courses, the number of semesters or fields of specialization.

+ The software can relatively flexibly be adapted, via yaml parameters and markdown forms and reports.

## Installation of a local test version

If you are interested in customizing the software for your own department, it is probably best to download a local version on your own computer. Try to get it run and make the customization work. If that works you can test and deploy it on a webserver using a docker container as explained in a subsequent section.

I recommend to use RStudio (https://www.rstudio.com/) for your local development.

### Install R packages

If you use docker on your local computer, you can install the docker image skranz/seminarmatching as explained in the server installation. You can even use RStudio via a web browser to test and modify the software inside the docker container.

If you don't use docker on your local computer, you first need to install all R packages. That is a little bit tedious, since the SeminarMatching package relies on several R packages written by me that are only hosted on Github and not on CRAN. Automatic installation of dependencies hosted on Github is not as well developed, as for CRAN packages, however. Below is a script that (hopefully) installs all needed packages (and probably some more, because I was to lazy to find the minimal required set). Just try to run it in R:

```r
library(methods)

# path to which you want to install
path = .libPaths()[1]
# shall existing packages be overwritten
glob.overwrite = FALSE

success = failed = NULL
from.cran = function(pkg, lib = path, overwrite = glob.overwrite,...) {
  if (!overwrite) {
    if (require(pkg,character.only = TRUE)) {
      cat("\npackage ",pkg," already exists.")
      return()
    }
  }
  res = try(install.packages(pkg, lib=lib))
  if (require(pkg,character.only = TRUE)) {
    success <<- c(success,pkg)
  } else {
    failed <<- c(failed,pkg)
  }
}

from.github = function(pkg, lib = path, ref="master", overwrite = glob.overwrite,upgrade_dependencies = FALSE,...) {
  repo = pkg
  pkg = strsplit(pkg,"/",fixed=TRUE)[[1]]
  pkg = pkg[length(pkg)]

  if (!overwrite) {
    if (require(pkg,character.only = TRUE)) {
      cat("\npackage ",pkg," already exists.")
      return()
    }
  }

  library(devtools)
  res = try(
  with_libpaths(new = path,
    install_github(repo,ref = ref,upgrade_dependencies = upgrade_dependencies,...)
  ))
  if (require(pkg,character.only = TRUE)) {
    success <<- c(success,pkg)
  } else {
    failed <<- c(failed,pkg)
  }

}

from.cran("shiny", lib=path)
from.cran("RCPP", lib=path)
from.cran("devtools", lib=path)

from.cran("curl",lib = path)
from.cran("openssl",lib = path)
from.cran("roxygen2", lib=path)
from.cran("dplyr",lib = path)

from.cran("knitr",lib = path)
from.cran("shinyjs",lib = path)
from.cran("V8",lib = path)
from.cran("rmarkdown", lib=path)

from.cran("rJava",lib = path)
from.cran("mailR",dep=TRUE, lib=path)

from.cran("shinyBS",lib = path)
from.cran("shinyAce",lib = path)

from.cran("RColorBrewer",lib = path)
from.cran("memoise", lib=path)

from.cran("mime", lib=path)

from.cran("xtable", lib=path)


from.cran("tidyr",lib=path)


# Install github packages
from.github(lib=path,"rstats-db/DBI",ref = "master")
from.github(lib=path,"rstats-db/RSQLite",ref = "master")

from.github(lib=path,"skranz/restorepoint",ref = "master")
from.github(lib=path,"skranz/stringtools",ref = "master")
from.github(lib=path,"skranz/codeUtils",ref = "master")
from.github(lib=path,"skranz/rmdtools",ref = "master")
from.github(lib=path,"skranz/dplyrExtras",ref = "master")
from.github(lib=path,"skranz/dbmisc",ref = "master")
#from.github(lib=path,"skranz/rowmins",ref = "master")

from.github(lib=path,"skranz/shinyEvents",ref = "master")
from.github(lib=path,"skranz/shinyEventsUI",ref = "master")
from.github(lib=path,"skranz/shinyEventsLogin",ref = "master")

from.github(lib=path,"skranz/TableTree",ref = "master")
from.github(lib=path,"skranz/YamlObjects",ref = "master")
from.github(lib=path,"skranz/shinyPart",ref = "master")
from.github(lib=path,"skranz/loginPart",ref = "master")
from.github(lib=path,"skranz/SeminarMatching",ref = "master")
```

In addition you probably want to download the source code of the SeminarMatching package from this Github page as a zip folder:

https://github.com/skranz/SeminarMatching/archive/master.zip 

You can then also locally build this package on your computer and see more details on how it works from the source code.

### Creating an example app

For the sake of brevity, I will refer to all files beyond the R packages that are needed to customize and deploy a working seminar matching software as the "app".

#### Copy example files

The folder `/examples` in this github repository contains skeletons of an example app that can be customized.

The subfolder `sem-shiny-server` contains brief files for shiny apps for the admin, teacher and students shiny apps.

The subfolder `sem-shared` contains folders for the databases, database schemas, yaml specification files and different RMarkdown files that can be adapted in order to customize reports and the user interfaces.

Best copy these two folders to some directory on your computer. In this guide we assume you have a Windows PC and that folder is `C:/sema`.

#### Create sqlite databases

The folder `sem-shared/db` will contain two SQLite databases: 
  - loginDB.sqlite containing login credentials for students and teachers
  - semDB.sqlite containing all other relevant data for the seminarmatching, like seminars, student informations, preference lists, or matching results.
  
We first have to create empty versions of these database. For this purpose adapt and run the following R code:

```r
# adapt working directory
library(SeminarMatching)
setwd("C:/sema/sem-shared")
db.dir = paste0(getwd(),"/db")


# Create loginDB.sqlite
logindb.arg = list(dbname=file.path(db.dir,"loginDB.sqlite"),drv=SQLite())
create.login.db(db.arg = logindb.arg)

# Insert a test user (make sure to delete him before production run)
create.user.in.db(userid = "test", email = "test",password = "test",db.arg = logindb.arg)

# Create semDB
schema.file = "./schema/semdb.yaml"
semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())
dbCreateSchemaTables(semdb, schema.file=schema.file,overwrite = FALSE)

# add test user to adminstaff
dbInsert(semdb, table="adminstaff",vals = list(userid="test",email="test"))

```

You can have now a look in the folder `sem-shared/db` and you see that the two files `loginDB.sqlite` and `semDB.sqlite` have been generated.

Now install some software like SQLiteStudio (https://sqlitestudio.pl/) to examine these databases, and possible to conduct some manual entries.

Note that we have generated a simple test user with id, email and password `test` to our loginDB and to the adminstaff table in semDB. The adminstaff table contains all users that are allowed to login as admin.

You should **definitely remove that test user** before going to production.

### Starting the admin interface

Now try to open the admin web interface by running the following lines of code:

```r
main.dir = "C:/sema/sem-shared"
app = AdminSeminarsApp(init.userid = "test", init.password="test", lang="en", main.dir = main.dir)
viewApp(app)
```

In the panel settings you can pick a semester in which the seminars take place and set a timeline for the matching process. Pick a semester and pick some dates. 

For testing, set for the moment the "Date from which on the semester will be shown as default for lecturers and students" and the "Date at which students start to see the seminars and can submit their preferences" be some dates in the past and the matching dates be some date in the future.

You have to enter dates in international date format, e.g.

2017-04-30

for the 30th of April 2017. Then save your settings by pressing submit.

Now stop the webapp (red stop button in the RStudio viewer or just close the tab in a browser) and start it again. You should know see a nice timeline for the seminar matching in the initial panel.

The set of semester and many other sets like courses of study etc, can be adapted by changing the file `sets.yaml` in the folder `sem-shared/yaml`. Also the other yaml files allow customization, but leave them in the moment as they are.

### Starting the teacher interface

To start the teacher interface that allows to add and modify seminars run the following code

```r
main.dir = "C:/sema/sem-shared"
app = EditSeminarsApp(init.userid = "test", init.password="test", lang="en", main.dir = main.dir)
viewApp(app)
```

After you login, you should get the following error message:
```
The user test has not been given any rights to edit seminars in any group.
```
Teachers will be assigned to groups (e.g. institutes). There can be several members in a a group that can edit seminars (professors, secretaries, teaching assisstants...), but each person can only be in one group. The first members of groups have to be added manually in the table `groupstaff` in semDB. You can do this manually with SQLLiteStudio or run the following code:

```r
db.dir = paste0(main.dir,"/db")
semdb = dbConnect(dbname=paste0(db.dir,"/semDB.sqlite"), drv = SQLite())
dbInsert(semdb,"groupstaff",list(userid="test",groupid="myinstitute",email="test",edit_Sem=TRUE, notify=TRUE, admin=TRUE, boss=TRUE))
```
Now try starting the app again and it should work.

Under the panel "Group Staff" you can now add additional users with rights to edit seminars or create new users in your group. The different permission fields are explained. A user with permission "boss" can not be deleted via the app but only with direct database access. 
In Ulm, I first manually added into the database the professors as bossess of their institute and let them later add additional users in a decentralized fashion.

In the panel "Seminars", you can create a new seminar for the current semester. Try it out.

The different form fields can be customized by modifying the corresponding yaml files in the folder `sem-shared/yaml`. When you change the fields, you also have to change the semDB database. For this purpose, you have to adapt the schema file `sem-shared/schema/semdb.yaml` and then recreate the database in a similar fashion as it was originally created. Note that some field names are hardcoded in the program and cannot be changed, e.g. "active" or "semname". These are fields with should be relevant for any university. So just don't change field names just for the fun of it.

I am aware that my documentation on customization is fairly sparse here, but I hope a bit try and error makes this work.

### Starting the student interface

TO BE CONTINUED...

## Installation on a webserver via Docker

## Timeline of seminar matching for a semester

- Adminstrator specifies all relevant dates for a given semester. This can be done quite some time ahead. The dates are saved in the `admin` database table.

- `default_start_date`: From this date on, the seminar will be the default seminar in teachers' and students' web apps.

- Until one day before `stud_start_date`: Lecturers can enter and activate seminars for the current semester.
    - Somebody should send reminder emails at `default_start_date` and one week before `stud_start_date`.

- From `stud_start_date` to `matching1_start_date-1` students should enter their (ordinal) preferences over seminars.
    - Lecturers can still change seminar settings like the number of slots, or weblinks. They should avoid to deactivate seminars, however.

- On `matching1_start_date` global random priority points `r_i` will be drawn and from a uniform distribution between 0 and 10 and stored for each student $i$.
    - Extension: One can customize the software such that students who add a sufficiently large number `n_i` of seminars to their preference list, are 'insured' against too low priorities. The actual random priority points will be `max(r_i, r_min[n_i])`, where `r_min[n]` is increasing in the number of selected seminars. The idea is that those students who add a large list of seminars really need a seminar, and should be likely to get a slot. More precisely, we want to make it likely that students who are willing to take say 50% of all offered seminars, get a slot.
    
        Truthful revelation is partly destroyed by such an extension. Students may be incentivized to add longer seminar lists than they actually want. Yet, if we can impose some penalty if students don't take an assigned seminar, we may counterveil such incentives. Fortunately, there are still no incentives to modify the ranking of seminars. 

- From `matching1_start_date` to `matching1_date-1`, lecturers see for each of their seminars a list of all students that put that seminar in their preference ordering. The students are ordered by their automatically computed priorities. Lecturers can adapt students' priorities and may also increase the number of seminar slots.
    - Lecturers see background data of students, like subject, specialization or taken courses
    - Lecturers DON'T see how a student has ranked their seminar (otherwise truthful ranking may no longer be incentive compatible for students, given that lecturers may adapt their priorities based on a student's ranking).
    - Lecturers also see some aggregate statistics that may help their decision of whether to expand their number of seminar slots:
        - How many students chose their seminar as 1st, 2nd or 3rd choice.
        - How many students would be matched if matching took place with current priorities.

- On `matching1_date` the actual seminar matching will take place (with a chromtab job or the admin) using the updated priorities. After the matching has been conducted, students and lecturers can see their results.

- From `matching1_date` to `matching2_start_date-1` students can enter their priorities over seminars for the 2nd matching round.

- The second matching round then proceeds in a similar spirit than the first matching round. Yet, there are no particular information shown that is relevant for increasing the number of slot. Slot increases should be done in the first round.

- After both matching rounds are conducted, lecturers can contact students for a meeting to assign topics. Topic assignment is not yet part of this software.


### Further aspects

### Negative Autocorrelation in Random Priority Points

Often students have to participate in more than one seminar. It seems a bit unfair, if one student draws in every semester a low random priority. To counterveil this effect, we can include some negative autocorrelation between random priorities. If you had a high priority last time, you are more likely to get low priority this time, and vice versa. The relant lines of codes are the following:

```
   # give a bonus if last time random points were below 5
    if (stud$random_points < 5) {
      stud$random_points = runif(1,5-stud$random_points,10)
    # give a malus if last time random points were above 5
    } else {
      stud$random_points = runif(1,0,15-stud$random_points)
    }
```
