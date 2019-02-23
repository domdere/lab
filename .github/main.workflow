workflow "Comment Hooks" {
  on = "issue_comment"
  resolves = ["bump-submodules"]
}

action "bump-submodules" {
  uses = "domdere/git-submodule-action@master"
  secrets = ["GITHUB_TOKEN"]
}

workflow "Code Checks" {
  on = "push"
  resolves = ["Haskell Linter"]
}

action "Haskell Linter" {
  uses = "domdere/haskell-lint-action@be45eaa"
  secrets = ["GITHUB_TOKEN"]
}
