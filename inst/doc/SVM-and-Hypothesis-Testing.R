## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", cache = FALSE, 
                      autodep = FALSE, cache.comments = FALSE,
                      fig.width = 8, fig.height = 5)

suppressPackageStartupMessages(library(svm01xht))


## ----1, include = TRUE--------------------------------------------------------
library(svm01xht)

dataset = loan_dataset
names(dataset)
dataset[sample(1:nrow(dataset), 8), ]


## ----2, include = TRUE--------------------------------------------------------
plot_hist(dataset, "person_age")
plot_hist(dataset, "person_education")
plot_hist(dataset, "loan_amnt")


## ----3, eval = FALSE----------------------------------------------------------
# lapply(names(dataset), function(x) {
#   plot_hist(dataset, x)
# })
# 

## ----4, include = TRUE--------------------------------------------------------
dataset = dataset[, -which(names(dataset) %in% c("loan_intent"))]


## ----5, include = TRUE--------------------------------------------------------
# Create a list which maps the character feature columns to numeric.
mapping = list(person_gender = c("male", "female"),
                person_education = c("High School", "Bachelor", "Master", "Doctorate", 
                                     "Associate"),
                person_home_ownership = c("OTHER", "RENT", "MORTGAGE", "OWN"),
                previous_loan_defaults_on_file = c("No", "Yes"))

dataset = preprocess(dataset, mapping, "loan_status")

dataset[sample(1:nrow(dataset), 8), ]


## ----6, include = TRUE--------------------------------------------------------
data = dataset[, c(1:12)]
label = dataset[, 13]

random.train.index = sample(1:nrow(data), size = 200, replace = FALSE)

linear.model = svm(data[random.train.index, ], label[random.train.index])
linear.model


## ----7, include = TRUE--------------------------------------------------------
linear.model.partial = svm(data, label, n = 200, features = names(data)[1:10])
linear.model.partial


## ----8, include = TRUE--------------------------------------------------------
poly.model = svm(data, label, type = "polynomial", n = 200)

rbf.model = svm(data, label, type = "rbf", n = 200)


## ----9, include = TRUE--------------------------------------------------------
random.test.index = sample(1:nrow(data), size = 200, replace = FALSE)

predict(linear.model, data[random.test.index, ])
test_accuracy(linear.model, data[random.test.index, ], label[random.test.index])

predict(linear.model.partial, data[random.test.index, ])
test_accuracy(linear.model.partial, data[random.test.index, ], label[random.test.index])

predict(poly.model, data[random.test.index, ])
test_accuracy(poly.model, data[random.test.index, ], label[random.test.index])

predict(rbf.model, data[random.test.index, ])
test_accuracy(rbf.model, data[random.test.index, ], label[random.test.index])


## ----10, include = TRUE-------------------------------------------------------
select_correlated_features(data, label, n = 8, show = TRUE)
select_correlated_features(data, label, min = 0.1)

data = data[, c(select_correlated_features(data, label, n = 8))]


## ----11, include = TRUE-------------------------------------------------------
random.train.index = sample(1:nrow(data), size = 500, replace = FALSE)
random.test.index = sample(1:nrow(data), size = 500, replace = FALSE)

tune_parameters(data[random.train.index, ], label[random.train.index], 
               data[random.test.index, ], label[random.test.index], 
               type = "linear", C = c(1, 10))

tune_parameters(data[random.train.index, ], label[random.train.index], 
               data[random.test.index, ], label[random.test.index], 
               type = "polynomial", c = c(1, 10), degree = c(2, 3))

tune_parameters(data[random.train.index, ], label[random.train.index], 
               data[random.test.index, ], label[random.test.index], 
               type = "rbf", C = c(1, 10), gamma = c(0.1, 1))


## ----12, include = TRUE-------------------------------------------------------
random.test.index = sample(1:nrow(data), size = 1000, replace = FALSE)

linear.model.tuned = svm(data, label, type = "linear", n = 1000, C = 10)
test_accuracy(linear.model.tuned, data[random.test.index, ], label[random.test.index])

poly.model.tuned = svm(data, label, type = "polynomial", n = 1000, c = 10, degree = 3)
test_accuracy(poly.model.tuned, data[random.test.index, ], label[random.test.index])

rbf.model.tuned = svm(data, label, type = "rbf", n = 1000, C = 1, gamma = 0.1)
test_accuracy(rbf.model.tuned, data[random.test.index, ], label[random.test.index])


## ----13, include = TRUE-------------------------------------------------------
linear.model.z.test = hypo_test(linear.model.tuned, 
                                data[random.test.index, ], label[random.test.index], 
                                0.85, alternative = "greater", method = "z")
linear.model.lrt = hypo_test(linear.model.tuned, 
                             data[random.test.index, ], label[random.test.index], 
                             0.85, alternative = "greater", method = "lrt")
linear.model.z.test$status
linear.model.lrt$status

rbf.model.z.test = hypo_test(rbf.model.tuned, 
                             data[random.test.index, ], label[random.test.index], 
                             0.85, alternative = "greater", method = "z")
rbf.model.lrt = hypo_test(rbf.model.tuned, 
                          data[random.test.index, ], label[random.test.index], 
                          0.85, alternative = "greater", method = "lrt")
rbf.model.z.test$status
rbf.model.lrt$status


## ----14, include = TRUE-------------------------------------------------------
visualize_ht(linear.model.z.test)
visualize_ht(linear.model.z.test, type = "II")
visualize_ht(linear.model.lrt)
visualize_ht(rbf.model.z.test)
visualize_ht(rbf.model.z.test, type = "II")
visualize_ht(rbf.model.lrt)


