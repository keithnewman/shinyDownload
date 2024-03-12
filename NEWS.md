# shinyDownload 0.3.8 (2024-03-12)

## BREAKING CHANGES

- The example apps no longer use `shinyUI()` and `shinyServer()`.
Consequently, the dependency on {shiny} is now version >= 0.10.

## Internal changes

- Remove usage of `:::` by creating the `<option>`{.html} tags manually.
- Package now undergoes CI checks on Github Actions.

# shinyDownload 0.3.7 (2023-05-06)

- Add a NEWS.md file.
- Add R project files.
- chore: Lint package.