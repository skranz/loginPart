examples.make.user = function() {
  random.password(nchar=6)
}


lop.crem = function() {
  list(
    title = "<h3>Create new user account</h3>",
    info1.label = "",
    info2.label = "",
    user.label="User",
    password.label="Password",
    email.label ="Email",
    create.btn.label="Send email to confirm account",
    cancel.btn.label="Cancel",

    user.inp="lopCreateUser",
    email.inp="lopCreateEmail",
    password.inp="lopCreatePassword",
    create.btn = "lopCreateBtn",
    cancel.btn = "lopCancelBtn",
    info="lopCreateInfo"
  )
}


lop.reset = function() {
  list(
    title = "<h3>Reset password for user account</h3>",
    info1.label = "",
    info2.label = "",
    user.label="User",
    password.label="Password",
    email.label ="Email",
    create.btn.label="Send email to confirm reset account.",
    cancel.btn.label="Cancel",

    user.inp="lopCreateUser",
    email.inp="lopCreateEmail",
    password.inp="lopCreatePassword",
    create.btn = "lopCreateBtn",
    cancel.btn = "lopCancelBtn",
    info="lopCreateInfo"
  )
}


lop.reset.email.user.ui = function(lop, ...) {
  restore.point("lop.reset.email.user.ui")
  copy.into.env(source = lop$reset)
  sel = ids2sel(cid(email.inp,lop))

  widgets = list(
    HTML(title),
    flexTextInput(cid(email.inp,lop), email.label, value = ""),
    #passwordInput(password.inp, password.label, value = ""),
    actionButton(cid(create.btn,lop), create.btn.label,"data-form-selector"=sel),
    actionButton(cid(cancel.btn,lop), cancel.btn.label),
    uiOutput(cid(info,lop))
  )
  ui = wellPanel(widgets)
  setUI(gid(info,lop),"")

  partButtonHandler(create.btn,pa=lop,create.email.user.click, lop=lop,no.authentication.required = TRUE)
  partButtonHandler(cancel.btn,pa=lop,cancel.create.email.user.click, lop=lop,no.authentication.required = TRUE)
  ui
}




lop.create.email.user.ui = function(lop, ...) {
  restore.point("lop.create.email.user.ui")
  copy.into.env(source = lop$crem)

  sel = ids2sel(cid(email.inp,lop))
  widgets = list(
    HTML(title),
    flexTextInput(cid(email.inp,lop), email.label, value = ""),
    #passwordInput(password.inp, password.label, value = ""),
    actionButton(cid(create.btn,lop), create.btn.label, "data-form-selector"=sel),
    actionButton(cid(cancel.btn,lop), cancel.btn.label),
    uiOutput(cid(info,lop))
  )
  ui = wellPanel(widgets)
  setUI(gid(info,lop),"")

  partButtonHandler(create.btn,pa=lop,create.email.user.click, lop=lop,no.authentication.required = TRUE)
  partButtonHandler(cancel.btn,pa=lop,cancel.create.email.user.click, lop=lop,no.authentication.required = TRUE)
  ui
}

create.email.user.click = function(lop, passwd.len=6,formValues,...) {
  copy.into.env(source = lop$crem)
  user = email = tolower(formValues[[gid(email.inp,lop)]])
  #user = email =  partValue(email.inp,lop)
  restore.point("create.email.user.click")

  if (is.null(lop$smtp)) {
    warning("lop$smtp not initialized")
    return(NULL)
  }

  if (!is.null(lop$check.email.fun)) {
    res = lop$check.email.fun(email)
    if (!res$ok) {
      show.html.warning(gid(info,lop),res$msg)
      return(NULL)
    }
  }


  link = lop.create.link(userid=user,link_type="confirm",lop=lop)

  res = lop$email.text.fun(lop,email,link)
  subject = res$subject; body = res$body; msg = res$msg




  use.mailr = TRUE
  if (!isTRUE(try(require(mailR)))) {
    # try sendmailR
    if (!isTRUE(try(require(sendmailR)))) {
      show.html.message("Neither mailR nor sendmailR package are installed! Cannot send email for sign-up.")
      return()
    }
    use.mailr = FALSE
  }
  if (use.mailr) {
    mail = c(list(subject=subject,body=body,to=email), lop$smtp)
    res = try(do.call(mailR::send.mail, mail))
  } else {
    res = try(sendmailR::sendmail(from=lop$smtp$from, to=email, subject=subject, msg=body,control=list(smtpServer=lop$smtp$host)))
  }

  if (is(res,"try-error")) {
    show.html.message("An error occured while trying to send the sign-up email.")
    return()
  }

  show.html.message(gid(info,lop),msg)
}


cancel.create.email.user.click = function(lop, ...) {
  restore.point("cancel.create.email.user.click")

  showPart(lop,container.id = lop$.container.id, ui.id=1)
}


lop.create.link = function(userid,link_type="confirm", lop=get.lop(), valid_secs=60*60*24*7) {
  restore.point("lop.create.link")

  linkid = random.password(nchar = 40)
  url = paste0(lop$app.url,"/?confirm=",linkid)
  create_time = Sys.time()
  valid_until = Sys.time() + valid_secs

  link = nlist(linkid, userid,link_type, url, create_time, valid_until, valid_secs)

  dbInsert(lop$conn,"loginlinks",link, mode="insert")

  link
}


default.email.text.fun = function(lop, email,link,...) {
  subject = paste0("Confirm user account for ", lop$app.title)

  body = paste0("
Hi,

you get this email, because you want to sign-up on ",lop$app.title," with this email adress. To confirm your user account and to choose a password, please follow the link below:\n\n ", link$url,
"\n\nIf you have not registred on ",lop$app.title,", someone else unsuccessfully tried to sign up with your email address. Then please ignore this email."
  )

  msg = paste0("I have send a confirmation email to ", email," from ",lop$smtp$from,".<br>The email contains a link to generate a password and activate your account.")

  nlist(subject, body, msg)
}
