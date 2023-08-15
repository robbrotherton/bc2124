if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
  quit()
} else {
  system("quarto render syllabus_pdf.qmd")
}
