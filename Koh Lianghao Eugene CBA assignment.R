setwd("~/Documents/NTU STUDY MATERIALS/Year 2 Sem 1/BC2406/Assignments/CBA")

library(data.table)
library(rgl)
library(ggplot2)
cba <- fread("marun_sample2.csv", stringsAsFactors = T)
#Factoring the categorical variable
cba$Formation <- factor(cba$Formation)

#filling up the NA values with the mean
for (col in colnames(cba)) {
  if (is.numeric(cba[[col]])) {
    # Calculate the mean for the column
    col_mean <- mean(cba[[col]], na.rm = TRUE)
    
    # Replace missing values with the calculated mean
    cba[[col]] <- ifelse(is.na(cba[[col]]), col_mean, cba[[col]])
  }
}

summary(cba)


#Q1 
#Notable Finding 1
library(gridExtra)
#One possible finding
ggplot(data = cba, aes(x=cba$`Mud pressure (psi)`, y=cba$MUDLOSSU)) + geom_point()
#Possible reasoning
plot1 <- ggplot(data = cba, 
       aes(x=cba$Formation,y=cba$MUDLOSSU)) +
  geom_boxplot()
plot2 <- ggplot(data = cba, 
                aes(x=cba$Formation,y=cba$`Mud pressure (psi)`)) +
  geom_boxplot()
grid.arrange(plot1, plot2, ncol = 2)

plot <- ggplot(data = cba, aes(x = cba$`Mud pressure (psi)`, y = cba$MUDLOSSU)) +
  geom_point()


ggplot(data = cba, aes(x=cba$Formation,y=cba$`Mud pressure (psi)`)) + geom_bar(stat = "identity") +
  labs(x = "Formation", y = "Mud Pressure (PSI)", title = "Mud Pressure vs Formation")

#Notable Finding 2
ggplot(data = cba, aes(x = cba$MFVIS, y = cba$`Mud pressure (psi`)) + geom_bar(stat = "identity")

#Notable Finding 3
library(scales)
p <- ggplot(data = cba, aes(x = `Depth (ft)`, y = `Fracture pressure`, group = Formation)) +
  geom_step(aes(color = "Fracture Pressure")) +
  facet_grid(~ Formation, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Depth (ft)") +
  ylab("Fracture/Pore Pressure")

# Add the secondary y-variable
p <- p + geom_step(aes(x = `Depth (ft)`, y = `Pore pressure`, color = "Pore Pressure", linetype = "dashed"))

# Apply scale transformation to secondary y-axis
p <- p + scale_color_manual(values = c("Fracture Pressure" = "blue", "Pore Pressure" = "red"))

p

#Plotting correlation 
library(corrplot)
library(RColorBrewer)
library(car)
library(dplyr)
numeric_data <- select_if(cba, is.numeric)
M <- cor(numeric_data, use = "pairwise.complete.obs")
corrplot(M, type = "upper", order = "hclust",
         col = brewer.pal(n = 8, name = "RdYlBu"))


#Q3 & Q4
library(caTools)
library(car)
#Finding for multi-collinearity
library(corrplot)
vif_values <- vif(lm(MUDLOSSU ~., data = cba))
print(vif_values)
m2 <- lm(MUDLOSSU ~Formation +  `Pore pressure`
         + `Hole size (in)` + METERAGE + DRLTIME + WOB + `Pump flow rate` + `Pump pressure` + MFVIS + RETSOLID + MIN10GEL +
           RPM, data = cba)
summary(m2)
set.seed(2)
#Linear Model
#train_set prediction
train <- sample.split(Y = cba$MUDLOSSU, SplitRatio = 0.7)
trainset <- subset(cba, train == T)
testset <- subset(cba, train == F)
#Linear regression model
m1 <- lm(MUDLOSSU ~ Formation
         + `Hole size (in)` + METERAGE + DRLTIME + WOB + `Pump flow rate` + `Pump pressure` + MFVIS + RETSOLID + MIN10GEL +
           RPM, data = trainset)
summary(m1)
trainset$prediction <- predict(m1, newdata = trainset)
mean_squared_error <- mean((trainset$MUDLOSSU - trainset$prediction)^2)
sqrt(mean_squared_error)


#test_set prediction
#Linear regression model
testset$prediction <- predict(m1, newdata = testset)
mean_squared_error <- mean((testset$MUDLOSSU - testset$prediction)^2)
sqrt(mean_squared_error)



#Cart Model
library(caTools)
library(rpart)
library(rpart.plot)
#train set prediction
set.seed(2)
train <- sample.split(Y = cba$MUDLOSSU, SplitRatio = 0.7)
trainset <- subset(cba, train == T)
testset <- subset(cba, train == F)
cart_model <- rpart(MUDLOSSU ~ ., data = trainset)
prediction <- predict(cart_model, newdata = trainset)
m3 <- rpart(MUDLOSSU ~ ., data = trainset, method = 'anova',
            control = rpart.control(minsplit = 2, cp = 0))
rpart.plot(m3)
mean_squared_error <- mean((trainset$MUDLOSSU-prediction)^2)
rmse <- sqrt(mean_squared_error)
rmse
#trainset RMSE:104.6169

CVerror.cap <- cart_model$cptable[which.min(cart_model$cptable[,"xerror"]), "xerror"] + cart_model$cptable[which.min(cart_model$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart_model$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart_model$cptable[i,1] * cart_model$cptable[i-1,1]), 1)
#get the trained model
m.opt <- prune(cart_model, cp= cp.opt)
rpart.plot(m.opt)
plotcp(m.opt)
printcp(m.opt)
m.opt$variable.importance

#test set prediction
predictions <- predict(cart_model, newdata = testset)
mean_squared_error <- mean((testset$MUDLOSSU - predictions)^2)
rmse <- sqrt(mean_squared_error)
rmse
#RMSE:113.231




























