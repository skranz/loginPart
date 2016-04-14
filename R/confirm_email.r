examples.make.user = function() {
  random.password(nchar=5, chars=letters)
}


lop.crepa = function() {
  crepa = list(
    info1 = "<p>You can set a randomly generated password for your account. Press accept to set the generated password.</p>",
    email.label ="Email",
    create.passwd.btn.label="Generate a password",
    cancel.btn.label="Cancel",

    "passwd.display"="show.password.inp",
    create.btn = "create.passwd.btn",
    accept.btn = "accept.passwd.btn",
    cancel.btn = "cancel.passwd.btn",
    info="create.passwd.info"
  )
  crepa
}

show.confirm.email = function(lop,linkid, app=getApp(),...) {
  restore.point("show.confirm.email")
  showPart(lop,container.id = "mainUI",ui.id="create.password")

  link = dbGetRow(conn = lop$conn,table = "loginlinks",params = list(linkid=linkid))
  if (is.null(link)) {
    ui = list(
      HTML("<p> The confirmation code in the link is not valid </p>")
    )
    setUI(lop$.container.id, ui)
  } else {
    app$is.authenticated = TRUE
    link = as.list(link[1,,drop=FALSE])
    lop$userid = link$userid
    lop$email = link$userid
    lop$linkid = link$linkid
    showPart(lop,ui.id = "create.password")
  }

}

lop.create.passwd.ui = function(lop, ...) {
  restore.point("lop.create.passwd.ui")
  copy.into.env(source = lop$crepa)

  userid = lop$userid
  email=userid


  widgets = list(
    h4(lop$app.title),
    HTML(paste0("<p>Create password for ",userid,"</p><br>")),
    uiOutput(cid("passwd.display",lop)),
    actionButton(cid(create.btn,lop), "Redraw password"),
    actionButton(cid(accept.btn,lop), "Accept password and login"),
    actionButton(cid(cancel.btn,lop), "Cancel and remove link"),
    uiOutput(cid(info,lop))
  )
  ui = wellPanel(widgets)
  setUI(gid("passwd.display",lop),"")
  setUI(gid("info",lop),"")
  #app = eventsApp()
  generate.passwd.click(lop)
  partButtonHandler(create.btn,pa=lop,generate.passwd.click, lop=lop)
  partButtonHandler(accept.btn,pa=lop,accept.passwd.click, lop=lop)
  partButtonHandler(cancel.btn,pa=lop,cancel.passwd.click,lop=lop)

  #check.ui(ui)
  ui
}

generate.passwd.click = function(lop, ..., app=getApp()) {
  restore.point("generate.passwd.click")

  lop$passwd = random.password(nchar=5, chars=letters)
  setUI(gid("passwd.display",lop), HTML(
    paste0("<p>Password: <b>", lop$passwd,"</b></p>")
  ))
}

accept.passwd.click = function(lop, app=getApp(),...) {
  restore.point("accept.passwd.click")

  #cat("\n\npaswwd: ", lop$passwd)
  salt = make.salt()
  res = make.password.hash(password = lop$passwd, salt=salt)
  restore.point("accept.passwd.click2")

  # Remove comment: for debugging purposes password will be stored
  #user = list(userid=lop$userid, email=lop$email, salt=salt, hash=res$hash,passwd=lop$passwd, confirmed=TRUE, create_time=Sys.time())

  # Don't store password in database
  user = list(userid=lop$userid, email=lop$email, salt=salt, hash=res$hash, confirmed=TRUE, create_time=Sys.time())


  dbInsert(lop$conn, "users",user, mode="replace")

  # Remove link
  dbDelete(lop$conn,"loginlinks", list(linkid=lop$linkid))

  lop$login.fun(userid=user$userid, lop=lop)
}


cancel.passwd.click = function(lop, app=getApp(),...) {
  restore.point("cancel.passwd.click")
  # Remove link
  dbDelete(lop$conn,"loginlinks", list(linkid=lop$linkid))
  # Show login form
  showPart(lop, ui.id="login")
}

