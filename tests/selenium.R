library(RSelenium)
library(testthat)

patientFindElement = function(css, wait=0.1, max=4) {
  restore.point("patientFindElement")
  cat("\nTry to find ", css)
  start = as.numeric(Sys.time())
  end = start + max
  while(TRUE) {
    els = remDr$findElements(using="css",css)
    if (length(els)>0) return(els[[1]])
    if (end < as.numeric(Sys.time())) {
      stop(paste0("Timeout: could not find ", css))
    }
    Sys.sleep(wait)
  }
}

# some helper functions
getField = function(css, field="value", unlist=TRUE,...) {
  webElem = patientFindElement(css,...)
  res = webElem$getElementAttribute(field)
  if (unlist) return(unlist(res))
  res
}
sendKeys = function(css, text,...) {
  webElem = patientFindElement(css,...)
  if (!is.list(text)) text = list(text)
  webElem$sendKeysToElement(text)
}
clickElement=function(css,...) {
  webElem = patientFindElement(css,...)
  webElem$clickElement()
}


pJS <- phantom()
# note we are running here without a selenium server phantomjs is
# listening on port 4444
# in webdriver mode
numDr = 4

remDrs = vector("list", numDr)
for (i in 1:numDr) {
  remDrs[[i]] <- remoteDriver(browserName = "phantomjs")
  remDrs[[i]]$open()
}


appURL <- "http://127.0.0.1:4646"

ind = 1; indDr = 1
login.email.check = function(ind=1, indDr=(ind %% numDr)+1) {
  restore.point("login.email.check")

  cat("\nNew check ind=",ind, " remDrvInd=",indDr)
  remDr<<-remDrs[[indDr]]
  remDr$navigate(appURL)

  userid=paste0("_test_",ind)
  email = userid
  password = "test"

  # enter login data
  sendKeys("#loginPart__loginUser",userid)
  getField("#loginPart__loginUser","value")

  sendKeys("#loginPart__loginPassword",password)
  getField("#loginPart__loginPassword","value")

  clickElement("#loginPart__loginBtn")

  #webElems = remDr$findElements(using="css","h2")
  #unlist(lapply(webElems, function(x){x$getElementText()}))

  cat("\nTry to click link...")
  clickElement("#studTabsetPanel [data-value='studPanel']")

  cat("\nTry to get email...")
  shown.email = getField("#studform_email","value")

  print(c(email=email, shown=shown.email))
  if (!identical(email,shown.email)) stop("Emails differ!")
}

for (ind in 1:10) login.email.check(ind)

for (i in 1:numDr) {
  remDrs[[i]]$close()
}
