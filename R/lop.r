examples.loginPart = function() {

  set.restore.point.options(display.restore.point = TRUE)

  setwd("D:/libraries/loginPart")
  app = eventsApp()

  login.fun = function(app=getApp(),userid,lop=get.lop(),...) {
    cat("Successfully logged in as ", userid)
    setUI("mainUI", success.ui)
  }

  check.email.fun = function(email="",...) {
    restore.point("check.email.fun")
    if (!isTRUE(email=="sebastian.kranz@uni-ulm.de" |
                email=="sebkranz@gmail.com")) {
      return(list(ok=FALSE, msg="Please only send to your own email adresses!"))
    }
  list(ok=TRUE,msg="")
  }

  sender.file = "D:/libraries/loginPart/sender.txt"
  db.arg = list(dbname="testdb",drv=SQLite())
  #lop.create.db(db.arg,overwrite = TRUE)

  lop = loginPart(db.arg = db.arg, login.fun=login.fun, check.email.fun=check.email.fun,app.url="http://127.0.0.1:4915", app.title="Ulm-WiWi Seminarvergabe",container.id = "mainUI",
  init.userid = "", init.password = ""
  )
  set.lop( lop)
  lop.connect.db(lop=lop)
  lop$login$ui = lop.login.ui(lop)
  lop$smtp = lop.get.smtp()


  success.ui = wellPanel(
    actionButton("successBtn", "Success... log in again")
  )
  buttonHandler("successBtn", function(app,...) {
    show.html.message(lop$login$alert,"")
    setUI("mainUI",lop$login$ui)
  })

  appInitHandler(function(session,...) {
    initLoginDispatch(lop)
  })

  ui = fluidPage(uiOutput("mainUI"))
  app$lop = lop
  runEventsApp(app,ui = ui, launch.browser=rstudio::viewer)

}

loginPart = function(id="loginPart",db.arg=lop.db.arg(),conn=NULL,login.fun=NULL, signup.fun = default.signup.fun, reset.fun = default.reset.fun, check.email.fun=NULL, email.text.fun = default.email.text.fun, app.url = NULL, app.title=id, container.id = NULL, init.userid="", init.password="", email.domain=NULL, smtp=NULL, set.need.authentication = TRUE, app=getApp(),
    login = lop.login(...),
    crem = lop.crem(...),
    crepa = lop.crepa(...),
    reset = lop.reset(...),
    ...
)
{
  restore.point("loginPart")

  if (set.need.authentication)
    app$need.authentication = TRUE

  if (is.null(check.email.fun)) {
    if (!is.null(email.domain)) {
      check.email.fun = function(email,...) {
        check.email.domain(email, email.domain)
      }
    } else {
      check.email.fun = function(email,...) {
        list(ok=TRUE,msg="")
      }
    }
  }

  lop = list(
    app.title = app.title,
    app.url = app.url,
    db.arg = db.arg,
    conn = conn,
    login.fun = login.fun,
    signup.fun = signup.fun,
    reset.fun = reset.fun,
    check.email.fun = check.email.fun,
    email.domain = email.domain,
    email.text.fun = email.text.fun,
    login = login,
    crem = crem,
    crepa = crepa,
    reset = reset,
    init.userid = init.userid,
    init.password = init.password,
    smtp = smtp
  )
  if (!is.null(lop$sender.file)) {
    sender.txt = readLines(lop$sender.file)
    txt = poor.decrypt(sender.txt)
    lop$sender = yaml.load(txt)
  }



  lop  = shinyPart(id = id,container.id = container.id, fields=lop, ui.funs = list(
    login = lop.login.ui,
    signup.email = lop.create.email.user.ui,
    reset.email = lop.reset.email.user.ui,
    create.password = lop.create.passwd.ui
  ))
}

#' This function must be called in the initHandler of the app
initLoginDispatch = function(lop, container.id=lop$.container.id, app=getApp()) {
  restore.point("initLoginDispatch")
  session = app$session
  lop$.container.id = container.id
  observe(priority = -100,x = {
    query <- parseQueryString(session$clientData$url_search)
    restore.point("appInitHandler")
    if ("confirm" %in% names(query)) {
      show.confirm.email(lop=lop, linkid=query$confirm)
    } else {
      showPart(lop,container.id = container.id, ui.id=1)
    }
  })
}

get.lop = function(app=getApp(), field="..lop.LOGIN") {
  app[[field]]
}

set.lop = function(lop,app=getApp(), field="..lop.LOGIN") {
  app[[field]] = lop
}

lop.db.arg = function(dbname="testdb",drv=SQLite(),...) {
  args = list(...)
  fill.defaults(args, nlist(dbname,drv))
}

show.html.message = function(id,msg="") {
  cat("\nhtml.message: ", msg)
  setUI(id,HTML(msg))
}

show.html.warning = function(id,msg="", color="red") {
  cat("\nhtml.warning: ", msg)
  html = paste0('<bold><font color="',color,'">',msg,'</font></bold>')
  setUI(id,HTML(html))
}


default.signup.fun = function(lop,...) {
  restore.point("default.signup.fun")
  ui.id = "signup.email"
  partUI(lop,ui.id)
  showPart( lop,container.id = lop$.container.id, ui.id = ui.id)
}


default.reset.fun = function(lop,...) {
  restore.point("default.reset.fun")
  ui.id = "reset.email"
  partUI(lop,ui.id)
  showPart( lop,container.id = lop$.container.id, ui.id = ui.id)
}
