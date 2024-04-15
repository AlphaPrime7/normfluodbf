# On library attachment, print message to user.
.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "\n",
    "== Welcome to normfluodbf ===========================================================================",
    "\nIf you find this package useful, please leave a star:",
    "\n   https://github.com/AlphaPrime7/normfluodbf",
    "\n",
    "\nIf you want to support me in my learning and development journey:",
    "\n https://www.buymeacoffee.com/tingweiadeck",
    "\n",
    "\nIf you encounter a bug or want to request an enhancement please file an issue at:",
    "\n   https://github.com/AlphaPrime7/normfluodbf/issues",
    "\n",
    "\n",
    "\nThank you for using normfluodbf!",
    "\n"
  )

  packageStartupMessage(msg)
}


## normfluodbf - R package for analysis of liposome flux assays data
## Copyright (C) 2024 Tingwei Adeck

# A way to set/get global variables
.cache <- function() {
  .store <- new.env()
  list(
    get = function(y) .store[[y]],
    set = function(y, v) .store[[y]] <- v
  )
}
.globals <- .cache()
