render_loac_report <- function(dept,title) 
{
  rmarkdown::render(
    "./R/LOAC Dept Report.Rmd", output_dir = "./reports/", output_file = paste(dept, "LOAC", "Report.pdf", sep = "_"),
    params = list(dept = dept,
                  dept_title = title
    )
  )
}

render_loac_report("MECH","Department of Mechanical and Materials Engineering")
render_loac_report("ECE","Department of Electrical and Computer Engineering")
render_loac_report("PSYC","Department of Pyschology")

report_list <- data_frame(dept = c("MECH","ECE","PSYC"),
                          title = c("Department of Mechanical and Materials Engineering",
                                    "Department of Electrical and Computer Engineering",
                                    "Department of Psychology"))


