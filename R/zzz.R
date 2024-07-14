## normfluodbf - R package that Cleans and Normalizes FLUOstar DBF and DAT Files
## Copyright (C) 2024 Tingwei Adeck

.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "\n",
    "== Welcome to normfluodbf - Developed by Tingwei Adeck and Tesla Adeck (Jesus Followers) ===========================================================================",
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
