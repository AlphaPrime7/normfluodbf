# On library attachment, print message to user.
.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "\n",
    "== Welcome to normfluodbf ===========================================================================",
    "\nIf you find this package useful, please leave a star: ",
    "\n   https://github.com/AlphaPrime7/normfluodbf'",
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
