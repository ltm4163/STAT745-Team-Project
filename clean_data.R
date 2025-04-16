clean_titanic <- function(df, is_train = TRUE, train_levels = NULL) {
  df <- df %>%
    { if (is_train) mutate(., 
                           Survived = factor(Survived, levels = c(0,1), labels = c("No","Yes"))
    ) else . } %>%
    mutate(Pclass = factor(Pclass, levels = c(1,2,3), ordered = TRUE),
           Sex    = factor(Sex,    levels = c("male","female"))) %>%
    { 
      med_age <- median(.$Age, na.rm = TRUE)
      mutate(., Age = ifelse(is.na(Age), med_age, Age))
    } %>%
    { 
      med_fare <- median(.$Fare, na.rm = TRUE)
      tmp <- mutate(., Fare = ifelse(is.na(Fare), med_fare, Fare))
      mutate(tmp, LogFare = log1p(Fare))
    } %>%
    { 
      if ("Embarked" %in% names(.)) {
        tmp <- .
        tmp$Embarked[tmp$Embarked == ""] <- NA
        mode_port <- names(which.max(table(tmp$Embarked)))
        tmp$Embarked[is.na(tmp$Embarked)] <- mode_port
        tmp$Embarked <- factor(tmp$Embarked)
        tmp
      } else .
    } %>%
    mutate(
      Title = sub("^.*, (.*?)\\..*$", "\\1", Name),
      Title = case_when(
        Title %in% c("Mlle","Ms")                    ~ "Miss",
        Title %in% c("Mme","Countess","Lady","Dona") ~ "Mrs",
        Title %in% c("Capt","Col","Major","Dr","Rev",
                     "Don","Sir","Jonkheer","the Countess") ~ "Rare",
        TRUE ~ Title
      ),
      Title = factor(Title),
      FamilySize = SibSp + Parch + 1,
      AgeGroup = cut(Age,
                     breaks = c(0,12,19,40,60,Inf),
                     labels = c("Child","Teen","Adult","Middle_Aged","Senior"),
                     right = FALSE),
      AgeGroup = factor(AgeGroup,
                        levels = c("Child","Teen","Adult","Middle_Aged","Senior"),
                        ordered = TRUE)
    )
  
  # If test set, align factor levels to training
  if (!is.null(train_levels)) {
    for (nm in names(train_levels)) {
      df[[nm]] <- factor(df[[nm]], levels = train_levels[[nm]])
    }
  }
  
  return(df)
}
