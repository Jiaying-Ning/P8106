
DROP OUT ANALYSIS


```{r}

student_longititude = 
  student_performance %>%
  janitor::clean_names() %>%
  na.omit() %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    g1:g3,
    names_to = "exam_time", 
    values_to = "exam_score") %>%
  relocate(exam_time,exam_score,id)  %>%
  mutate(exam_time = recode(exam_time, "g1" =  "time1", "g2" = "time2", "g3"="time3"))

```


```{r}
student_longititude %>%
  ggplot( aes(exam_time, exam_score, group=id)) + geom_line() +   labs(title = "spaghetti plot") 
```

```{r}
student_longititude %>% 
  filter(exam_time != "time1" & exam_score == 0 )
```


student_longititude %>%
  filter(exam_score != 0 ) %>%
  ggplot( aes(exam_time, exam_score, group=id)) + geom_line() +   labs(title = "spaghetti plot") 
```


several interesting insight was found using the spaghetti plot:
  
  1. not all students make it to the end: some students drop out from the course after the first exam and second exam,
2. Variable "absence" is 0 for those who dropped out from courses.
3. if we ignore those who have 0 score on period 2 test and period 3 test, no clear population trend is observed by looking at the spaghetti plot, but we do observe different direction in slope.

cautious is needed when dealing with this situation and here are some of my consideration 
1. If we want to evaluate covariates that predicts better exam score, or average exam score,should students who drop out from the courses (those who have 0 scores in class 2 or/and class 3) be included in the model?

```{r}

student_performance = mutate(student_performance,drop_out = ifelse(G2==0 | G3==0,"dropped","stayed"), drop_out = as.factor(drop_out))

```



```{r}

cols <- c(3,7:8,13:15,24:31)
featurePlot(x = student_performance[,cols], 
            y = student_performance$drop_out,
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")),
            plot = "density", pch = "|", 
            auto.key = list(columns = 2))
```
```{r}
student_performance %>%
  group_by(drop_out) %>%
  drop_na(absences) %>%
  summarize(n_obs = n(),mean = mean(absences))
```
In here we also see that absences do not contain any value for those who dropped the course. 



```{r}
set.seed(62)

used = c(1:29,31,34)

student_performance_dropout = student_performance[,used]

rowTrain <- createDataPartition(y = student_performance_dropout$drop_out,
                                p = 0.75,
                                list = FALSE)
training = student_performance_dropout[rowTrain,]
testing = student_performance_dropout[-rowTrain,]
```

```{r}
contrasts(training$drop_out)

```


```{r}
# Dumy code categorical predictor variables
x <- model.matrix(drop_out~., training)[,-1]
# Convert the outcome (class) to a numerical variable
y <- ifelse(training$drop_out == "stayed", 1, 0)
```



```{r}
cv.lasso <- cv.glmnet(x, y, 
                      alpha = 1, 
                      lambda = exp(seq(-5, 0, length = 100)))
plot(cv.lasso)
abline(h = (cv.lasso$cvm + cv.lasso$cvsd)[which.min(cv.lasso$cvm)], col = 4, lwd = 2)

cv.lasso$lambda.min
```
```{r}
plot_glmnet(cv.lasso$glmnet.fit)

```
```{r,include = FALSE}

Lasso_coe = predict(cv.lasso, s = "lambda.min", type = "coefficients")

final_parameter=which(Lasso_coe[,1] != 0)
final_parameter

x.test <- model.matrix(drop_out ~., testing)[,-1]
Lasso_pred=predict(cv.lasso, newx = x.test, s = "lambda.min", type = "response")

```




```{r}
predicted.classes <- ifelse(probabilities > 0.5, "stayed", "dropped")

confusionMatrix(data = as.factor(predicted.classes),
                reference = testing$drop_out,
                positive = "stayed")
```

```{r}
glm.fit <- glm(drop_out ~ school+Mjob+failures+schoolsup+paid+higher+romantic+Walc+G1, 
               data = training, 
               family = binomial(link = "logit"))
summary(glm.fit)
```



After fitting the penalized parameter into logistic model, we see that factor: schoolsup(extra educational support), paid(extra paid classes within the course subject (Math or Portuguese)), walc(weekend alcohol consumption) and G1(grades in first period) are statistically significant predictors for drop_out. 

```{r}
exp(2.5239)
exp(1.1401)
exp(0.5486)
exp(0.5610)
```

Specifically, 
- Schoolsup:  those who have extra educational support's odds of staying in class is 12.47 times the odds of those who do not have extra educational support. 
  - paid:  those who have extra paid classess within the course subject's odds of staying in class is 3.127 times the odds of those who do not  have extra paid classess within the course subject.
- walc: with every level increase in alcohol consumption on weekend, a student will have 1.73 increase in odds of staying in class.  
- walc: with every level increase in alcohol consumption on weekend, a student will have 1.73 increase in odds of staying in class.  
- G1: with every unit increase in  the first period scorem students; odds of staying in class also increase 1.75 times. Among all, G1 is most statistically significant, which make it a strong predictor. This also make sense because people who have bad grade in their first exam might afraid to getting a really bad final report and thus loose the motivation to continue in class. 

