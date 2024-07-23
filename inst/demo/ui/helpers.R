helpPopup <- function(content, title = NULL) {
  a(href = "#",
    class = "popover-link",
    `data-toggle` = "popover",
    `data-title` = title,
    `data-content` = content,
    `data-html` = "true",
    `data-trigger` = "hover",
    icon("question-circle")
  )
}

withBusyIndicator <- function(button) {
  id <- button[['attribs']][['id']]
  tagList(
    button,
    span(
      class = "btn-loading-container",
      `data-for-btn` = id,
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    )
  )
}

db <- function() {
  user_base <- data.frame(
    user = c("user1", "adeck@brown.edu"),
    password = c("password1", "tingwei"), #purrr::map_chr(c("pass1", "pass2"), sodium::password_store) ; sapply(c("password1", "password2"), sodium::password_store)
    permissions = c("standard", "admin"),
    name = c("User One", "GODsFriend"),
    stringsAsFactors = FALSE
  )

  assign('user_base', user_base, envir = parent.frame())
  return(user_base)
}
