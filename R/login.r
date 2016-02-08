

lop.login = function(failed.fun=lop.failed.login, create.user.fun=NULL,...) {
  login = list(
    userid.label="user",
    password.label="password",
    login.btn.label="log in",
    signup.btn.label="sign up",
    reset.btn.label = "forgot password",

    userid.inp="loginUser",
    password.inp="loginPassword",
    login.btn = "loginBtn",
    signup.btn = "loginSignupBtn",
    reset.btn = "loginResetBtn",
    alert="loginAlert",
    failed.fun=failed.fun
  )
  login
}

lop.login.ui = function(lop,...) {
  copy.into.env(source = lop$login)


  widgets = list(
    textInput(cid(userid.inp,lop), userid.label, value = lop$init.userid),
    passwordInput(cid(password.inp,lop), password.label, value = lop$init.password),
    actionButton(cid(login.btn,lop), login.btn.label),
    actionButton(cid(signup.btn,lop), signup.btn.label),
    actionButton(cid(reset.btn,lop), reset.btn.label),
    uiOutput(cid(alert,lop))
  )
  ui = wellPanel(widgets)

  partButtonHandler(login.btn,pa=lop,lop.login.btn.click,lop=lop)
  partButtonHandler(signup.btn,pa=lop,lop.signup.btn.click,lop=lop)
  partButtonHandler(reset.btn,pa=lop,lop.reset.btn.click,lop=lop)
  ui
}

lop.signup.btn.click = function(app=getApp(),lop,...) {
  if (!is.null(lop$signup.fun)) {
    lop$signup.fun(lop=lop,...)
  }
}

lop.reset.btn.click = function(app=getApp(),lop,...) {
  if (!is.null(lop$reset.fun)) {
    lop$reset.fun(lop=lop,...)
  }
}



lop.login.btn.click = function(app=getApp(),lop,...) {
  login = lop$login
  userid = partValue(login$userid.inp,lop)
  password = partValue(login$password.inp,lop)

  res = lop.check.login(userid=userid,password = password, lop=lop)
  restore.point("lop.login.btn.click")
  if (res$ok==TRUE) {
    lop$login.fun(userid=userid, password=password, lop=lop)
  } else {
    login$failed.fun(userid=userid, password=password, msg=res$msg, lop=lop)
  }
}

lop.failed.login = function(app=getApp(),lop=get.lop(),msg,...) {
  login = lop$login
  show.html.warning(gid(login$alert,lop),msg)
  #createAlert(app$session, login$alert,title="Log-in failed",content=msg, style="warning")
  cat("\nlog-in failed: ",msg)
}

lop.check.login = function(userid, password, lop=get.lop()) {
  restore.point("lop.check.login")
  if (nchar(userid)==0)
    return(list(ok=FALSE,msg="No user name entered."))
  user = lop.get.user(userid=userid, lop=lop)
  if (NROW(user)==0) {
    return(list(ok=FALSE,msg="User does not exist."))
  }
  ok = check.password(password = password, salt=user$salt,hash=user$hash)
  if (ok) {
    return(list(ok=TRUE,msg=""))
  }
  return(list(ok=FALSE,msg="Wrong password."))
}


check.email.domain = function(email, domain) {
  ok = str.ends.with(email, domain)
  if (!ok) {
    return(list(ok=ok, msg=paste0("You can only create an account with an email that ends with ", domain)))
  }
  return(list(ok=ok, msg=""))
}



