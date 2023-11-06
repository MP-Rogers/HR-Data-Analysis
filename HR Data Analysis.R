lapply(c("tidyverse","scales","plotly","ggcorrplot","lubridate", "ggpubr"), library, character.only = TRUE)
Dataset <- read.csv("hr_dashboard_data.csv")

glimpse(Dataset)
#summary(Dataset)
Dataset$Joining.Date<-as_date(Dataset$Joining.Date)
numerics<-Dataset |> select_if(is.numeric) |> na.omit()
cor.matrix<-cor(numerics)
cor.plot<-ggcorrplot(cor.matrix, type = "lower", hc.order=TRUE, lab = TRUE, 
                     title = "Correlation Matrix of Human Resources Info")
print(cor.plot)

#projects completed is #/25, productivity, feedback and satisfaction is a score out of 100



projects.age.plot<-ggplot(Dataset, mapping = aes(x = Age, y = Projects.Completed))+
  geom_point(alpha= 0.8, colour = "green")+
  geom_smooth()+
  labs(title ="Plot of Age and Projects completed at a certain company")+
  theme(plot.title = element_text(hjust = FALSE))
print(projects.age.plot)

salary.age.plot<-ggplot(Dataset, mapping = aes(x = Projects.Completed, y = Salary, colour = Department))+
  geom_point(alpha= 0.8, size = 1.2)+
  labs(title ="Plot of Salary and Projects completed at a certain company")+
  scale_y_continuous(labels = dollar_format())+
  theme(plot.title = element_text(hjust = FALSE))
print(salary.age.plot)

Gender.Wage.Boxplot<- Dataset |> ggplot(mapping = aes(x = Gender, y = Salary, fill = Gender))+
  geom_boxplot(notch = TRUE)+
  scale_y_continuous(labels = dollar_format())+
  labs(title = "Boxplot showing salaries in a certain company by gender")
print(Gender.Wage.Boxplot)

gender.hist<-ggplot(Dataset, mapping = aes(y = Salary, fill = Gender))+
  geom_density(alpha = 0.5)
print(gender.hist)

date.salary<-ggplot(Dataset, mapping = aes(x = Joining.Date, y = Salary))+
  geom_point()
print(date.salary)

Department.Wage.Boxplot<- Dataset |> ggplot(mapping = aes(x = Department, y = Salary, fill = Department))+
  geom_boxplot(alpha = 0.8, notch = TRUE)+
  scale_y_continuous(labels = dollar_format())+
  labs(title = "Boxplot showing salaries in a certain company by department")
print(Department.Wage.Boxplot)

salary.age.eqn<-lm(Salary ~ Age, Dataset)
print(summary(salary.age.eqn))

Department.Satisfaction<-Dataset |> ggplot(mapping = aes(x = Department, y = (Satisfaction.Rate..../100), fill = Department))+
  geom_boxplot()+
  labs(title = "Job Satisfaction by Department")+
  ylab("Satisfaction Rate (%)")+
  scale_y_continuous(labels = percent_format())+
  theme(plot.title = element_text(hjust = 0.5))
Department.Satisfaction<-ggplotly(Department.Satisfaction)
print(Department.Satisfaction)
  
