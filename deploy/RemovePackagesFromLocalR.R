# Run this to remove obsolete packages from the local R installation that is needed as framework for
# the Fate Explorer installation.

# Development tools
remove.packages(pkgs = c("testthat", "tcltk", "tools", "codetools", "rstudioapi", "devtools", "gh", "git2r", "roxygen2"))

# Markdown stuff
remove.packages(pkgs = c("rmarkdown", "markdown", "knitr"))

# Various stuff that is not required
remove.packages(pkgs = c("survival", "xfun", "isoband", "highr"))
remove.packages(pkgs = c("curl", "praise", "rematch2"))
