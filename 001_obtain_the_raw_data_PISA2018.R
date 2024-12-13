library(learningtower)

full_student_data_2018 <- load_student(2018)

data(countrycode)
data(school)
school_2018=school[school$year==2018,]

saveRDS(full_student_data_2018,file = "./data/raw/full_student_data_2018.Rds")
saveRDS(countrycode,file = "./data/raw/countrycode.Rds")
saveRDS(school_2018,file = "./data/raw/school_2018.Rds")
