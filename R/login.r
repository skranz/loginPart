

lop.login = function(failed.fun=lop.failed.login, create.user.fun=NULL,...) {
  login = list(
    login.title = "",
    userid.label="user",
    password.label="password",
    login.btn.label="log in",
    signup.btn.label="sign up",
    reset.btn.label = "forgot password",
    login.help = "",

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


  sel = ids2sel(c(cid(userid.inp,lop),cid(password.inp,lop)))
  widgets = list(
    HTML(lop$login$login.title),
    textInput(cid(userid.inp,lop), userid.label, value = lop$init.userid),
    passwordInput(cid(password.inp,lop), password.label, value = lop$init.password),
    actionButton(cid(login.btn,lop), login.btn.label, "data-form-selector"=sel),
    actionButton(cid(signup.btn,lop), signup.btn.label),
    actionButton(cid(reset.btn,lop), reset.btn.label),
    uiOutput(cid(alert,lop)),
    HTML(lop$login$login.help)
  )
  ui = wellPanel(widgets)
  setUI(gid(alert,lop),"")

  partButtonHandler(login.btn,pa=lop,lop.login.btn.click,lop=lop,no.authentication.required = TRUE)
  partButtonHandler(signup.btn,pa=lop,lop.signup.btn.click,lop=lop,no.authentication.required = TRUE)
  partButtonHandler(reset.btn,pa=lop,lop.reset.btn.click,lop=lop,no.authentication.required = TRUE)
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



lop.login.btn.click = function(app=getApp(),lop,formValues,...) {
  login = lop$login
  userid = tolower(formValues[[gid(login$userid.inp,lop)]])
  password = formValues[[gid(login$password.inp,lop)]]

  cat("userid = ", userid, " password = ", password)
  #partValue(login$userid.inp,lop)
  #password = partValue(login$password.inp,lop)

  res = lop.check.login(userid=userid,password = password, lop=lop)
  restore.point("lop.login.btn.click")
  if (res$ok==TRUE) {
    app$is.authenticated = TRUE
    lop$login.fun(userid=userid, password=password, lop=lop)
  } else {
    app$is.authenticated = FALSE
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



