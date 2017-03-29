render_loac_report <- function(dept, title) 
{
  rmarkdown::render(
    "./templates/LOAC Dept Report.Rmd",
    output_dir = "reports/",
    output_file = paste(dept, "LOAC", "Report.pdf", sep = "_"),
    params = list(dept = dept,
                  dept_title = title
    )
  )
}

programs <- tibble(dept = c("DRAM", "PSYC", "PHYS", "CIVL","CHEE","ECE","ENPH","GEOE","MECH","MINE","MTHE"), 
                   title = c("Department of Drama", "Department of Psychology", "Department of Physics, Engineering Physics & Astronomy", "Deparment of Civil Engineering", "Department of Chemical Engineering and Engineering Chemistry", "Department of Electrical and Computer Engineering", "Department of Physics, Engineering Physics & Astronomy", "Department of Geological Science and Engineering", "Department of Mechanical and Materials Engineering", "Department of Mining Engineering", "Department of Mathematics and Statistics"))


pmap(programs, render_loac_report)
