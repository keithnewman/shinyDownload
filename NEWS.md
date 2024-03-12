# shinyDownload 0.4.0 (2024-03-12)

## New features

- New methods are now available:
    - `downloadGGPlotButtonServer()`
    - `downloadReportButtonServer()`
    - `downloadTableButtonServer()`
These methods allow for the module server to be more easily integrated into the
server code directly, rather than needing `callModule` like before. There are a
few key differences to this in terms of how the server logic would look, mainly
around where you pass a reactive *expression* instead of a reactive *value*.

## DEPRECATIONS

- The old methods:
    - `downloadGGPlotButton()`
    - `downloadReportButton()`
    - `downloadTableButton()`
Will still work as before for now, but will create a deprecation warning.
The old methods won't receive further updates. We recommend transitioning to the
newer layouts where possible, although this transition involves transitioning 
across a breaking change, so please test your app throughly. To help with the
transition, the examples have been updated. See `shinyDownloadExample()` for
updated examples of usage.

## Internal changes

- Documentation now allows markdown syntax.

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