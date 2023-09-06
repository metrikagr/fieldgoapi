#github commands in one script
git2r::add(path = getwd())
commit_msg <- "fix error in parsing config argument"
git2r::commit(message = commit_msg)
config(repo, user.name = "Alice", user.email = "alice@example.org")
git2r::config(user.name = "omarbenites",user.email = "omar_cs_7@hotmail.com")
git2r::push()
