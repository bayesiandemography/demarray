
context("helper-functions")

test_that("infer_dimtypes works as expected", {
    expect_identical(infer_dimtypes(c("age", "age5", "age10yr", "sex", "year")),
                     c("age", "age", "age", "state", "time"))
    expect_identical(infer_dimtypes(c("duration", "unknown")),
                     c("age", "state"))
    expect_identical(infer_dimtypes(c("start", "end")),
                     c("state", "state"))
    expect_identical(infer_dimtypes(c("stage", "parity")),
                     c("state", "state"))
    expect_identical(infer_dimtypes(c("reg_orig", "reg_dest", "birth cohort")),
                     c("origin", "destination", "cohort"))
    expect_identical(infer_dimtypes(c("reg_dest", "birth cohort")),
                     c("destination", "cohort"))
    expect_identical(infer_dimtypes(c("ethnicity_parent", "ethnicity_child")),
                     c("parent", "child"))
    expect_identical(infer_dimtypes(c("Lexis triangle", "Lexis Triangles", "triangle", "Triangles")),
                     rep("triangle", 4))
})
