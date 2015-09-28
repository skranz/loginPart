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

    user.inp="lop.create.user",
    email.inp="lop.create.email",
    password.inp="lop.create.password",
    create.btn = "lop.create.btn",
    cancel.btn = "lop.create.btn",
    info="lop.create.info"
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

    user.inp="lop.create.user",
    email.inp="lop.create.email",
    password.inp="lop.create.password",
    create.btn = "lop.create.btn",
    cancel.btn = "lop.create.btn",
    info="lop.create.info"
  )
}


lop.reset.email.user.ui = function(lop, ...) {
  restore.point("lop.reset.email.user.ui")
  copy.into.env(source = lop$reset)

  widgets = list(
    HTML(title),
    textInput(cid(email.inp,lop), email.label, value = ""),
    #passwordInput(password.inp, password.label, value = ""),
    actionButton(cid(create.btn,lop), create.btn.label),
    actionButton(cid(cancel.btn,lop), cancel.btn.label),
    uiOutput(cid(info,lop))
  )
  ui = wellPanel(widgets)

  partButtonHandler(create.btn,pa=lop,create.email.user.click, lop=lop)
  ui
}




lop.create.email.user.ui = function(lop, ...) {
  restore.point("lop.create.email.user.ui")
  copy.into.env(source = lop$crem)

  widgets = list(
    HTML(title),
    textInput(cid(email.inp,lop), email.label, value = ""),
    #passwordInput(password.inp, password.label, value = ""),
    actionButton(cid(create.btn,lop), create.btn.label),
    actionButton(cid(cancel.btn,lop), cancel.btn.label),
    uiOutput(cid(info,lop))
  )
  ui = wellPanel(widgets)

  partButtonHandler(create.btn,pa=lop,create.email.user.click, lop=lop)
  ui
}

create.email.user.click = function(lop, passwd.len=6,...) {
  copy.into.env(source = lop$crem)
  user = email = partValue(email.inp,lop)
  restore.point("create.email.user.click")

  if (is.null(lop$smtp)) {
    warning("lop$smpt not initialized")
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

  mail = c(list(subject=subject,body=body,to=email), lop$smtp)
  do.call(mailR::send.mail, mail)

  show.html.message(gid(info,lop),msg)
}


lop.create.link = function(userid,link_type="confirm", lop=get.lop(), valid_secs=60*60*24*7) {
  restore.point("lop.create.link")

  linkid = make.salt(20)
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
