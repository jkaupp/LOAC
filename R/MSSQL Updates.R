library(DBI)
library(RODBC)
library(magrittr)
library(dplyr)


con <- odbcConnect("FEAS", uid="AD\\kauppj", pwd="Laurenque5pge!")

# Write the student_info table
sqlSave(con, data.frame(master), tablename = "student_info", append = TRUE)

# Write the cla_plus table
sqlSave(con, data.frame(cla.data), tablename = "cla_plus", append = TRUE)

# Write the Cat table
sqlSave(con, data.frame(cat.data), tablename = "cat", append = TRUE)

# Write the VALUE table
sqlSave(con, data.frame(m.full.value), tablename = "value", append = TRUE)







