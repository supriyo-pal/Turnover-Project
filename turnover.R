library(ggplot2)
library(readxl)


data<-read.csv("C:/Users/KIIT/Desktop/Inueron Projects/Turnover Project/turnover-data-set.csv")

head(data)
#Summary Statistics
summary(data)

class(data)

#Age distribution
ggplot(data, aes(x=age)) + geom_histogram(binwidth=5) + ggtitle("Age Distribution")

names(data)

#Anxity Distribution
data|>
  ggplot(aes(x=anxiety))+geom_bar()+labs(title = "Anxity Distribution")


#Correlation between the variables
library(corrplot)
corr_matrix <- cor(data[, sapply(data, is.numeric)])
corrplot(corr_matrix, method="circle")

library(ggcorrplot)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)

#Logistic Regression
model <- glm(event ~ ., data = data, family = binomial)
summary(model)


library(rpart)
tree <- rpart(event ~ ., data = data, method = "class")
plot(tree)
text(tree)


#Anova analysis
anova_model <- aov(age ~ industry, data = data)
summary(anova_model)

#Chi squere Test
table(data$coach, data$event)
chisq.test(data$coach, data$event)

#Extraversion Vs Turnover
ggplot(data, aes(x=extraversion, fill=factor(event))) + geom_histogram(position="dodge") + ggtitle("Extraversion vs Turnover")

#Supervison Gender Vs Turnover
ggplot(data, aes(x=head_gender, fill=factor(event))) + geom_bar(position="dodge") + ggtitle("Supervisor Gender vs Turnover")

#Anxity vs Turnover
ggplot(data, aes(x=anxiety, fill=factor(event))) + geom_histogram(position="dodge") + ggtitle("Anxiety vs Turnover")

#Self control vs turnover
ggplot(data, aes(x=selfcontrol, fill=factor(event))) + geom_histogram(position="dodge") + ggtitle("Self-Control vs Turnover")


#Dashboard for checking the distribution of various numerical variables
library(shiny)
ui <- fluidPage(
  titlePanel("HR Turnover Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Variable:", choices=names(data))
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    ggplot(data, aes_string(x=input$variable)) + geom_histogram(binwidth=5) + ggtitle(paste(input$variable, "Distribution"))
  })
}

shinyApp(ui, server)



