output$pageStub <- renderUI(
  tagList(
    fluidPage(
      theme = shinytheme("sandstone"),
      h1("AsalView - Demo", align = "center"),
      tags$p(tags$i("Génesis Masioth Molina Rodríguez - Biotechnology Engineer"), align = "center"),
      tags$div(
        tags$ul(
          tags$li("text"),
          tags$li("text"),
          tags$li("text"),
          tags$li("text"),
          tags$li("text")
        )
      ),
      tags$a(href="?asalview-page", "Start Demo!")
    )
  )
)