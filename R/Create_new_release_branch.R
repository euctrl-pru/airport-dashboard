
new_release_branch=202203

shell("git checkout master")

shell("git pull")

# Create new release branch
shell(paste0("git checkout –b ", new_release_branch, "-release"))

# Push the new release branch and set the remote as upstream
shell("git push --set-upstream origin ", new_release_branch, "-release")