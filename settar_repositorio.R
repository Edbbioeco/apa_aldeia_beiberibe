# Pacotes ----

library(usethis)

library(gert)

# Iniciar git ----

usethis::use_git()

# Primeiro commit ----

## Adicionar ----

gert::git_add(files = ".gitignore")

## Commit ----

gert::git_commit(message = ".gitignore")

# Criar repositório ----

usethis::use_github()

