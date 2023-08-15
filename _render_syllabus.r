if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
  quit()
} else {
  system("quarto render _syllabus_pdf.qmd")
}
