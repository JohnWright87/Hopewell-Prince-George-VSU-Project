

library(ggplot2)
library(tidyverse)


######## Ocuppations Graphs 
Occupations <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Occupations.csv")

Occupations_long <- Occupations %>%
  gather(key = "County", value = "numOccupations", Crater.Area:Hopewell) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region")))

Occupations_long %>%
  group_by(Occupations, County) %>%
  ggplot(aes(x = reorder(Occupations, numOccupations), y = numOccupations, fill=County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = " Current Job Openings by Occupations and Locations",
       x = "Types of Occupations",
       y = "Number of Job Ads") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))

head(Occupations)

######################################################################################

Job.Titles <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Job Titles.csv")

Job.Titles_long <- Job.Titles %>%
  gather(key = "County", value = "numJobTitles", Crater.Area:Hopewell) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region")))

Job.Titles_long %>%
  group_by(Job.Titles, County) %>%
  ggplot(aes(x = reorder(Job.Titles, numJobTitles), y = numJobTitles, fill=County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = " Openings by Jobs Titles and Location",
       x = "Job Titles",
       y = "Number of Jobs Ads") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))

head(Job.Titles)


#######################################################################

Certifications <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Certifications.csv")
# convert the wide format to long format with a column called County
# gather is the opposite of separate and brings multiple columns into two columns
#   1. column names
#   2. culumn values
cert_long <- Certifications %>%
  gather(key = "County", value = "numCert", Crater.Area:Hopewell) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region")))

# to graph the data from above
#   1. group by the certificate and then county
#   2. geom_col is the same as geom_bar(stat = "identity")
#   3. position = "dodge" makes the bars side by side
#   4. coord_flip makes the graph horizontal
#   5. "labs" function allows you to add labels
#       NOTE: if you do not want the Cert. Names label you can use y = "" in labs
cert_long %>%
  group_by(Certificate.Name, County) %>%
  ggplot(aes(x = reorder(Certificate.Name, numCert), y = numCert, fill=County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Current Openings by Certification and Location",
       x = "Types of Certificates",
       y = "Number of Job Ads by Certifications") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))

########################################################################

############ Hards Skills

Hard.Skills <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Hard Skills.csv")

Hard.Skills_long <- Skill.Name %>%
  gather(key = "County", value = "numHardSkills", Crater.Area:Hopewell) %>%
mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region")))

Hard.Skills_long %>%
  group_by(Skill.Name, County) %>%
  ggplot(aes(x = reorder(Skill.Name, numHardSkills), y = numHardSkills, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Openings by Hard Skills and Location",
       x = "Skill Name",
       y = "Number of Job Ads") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Area"))


head(Hard.Skills)


##############################################################################

Soft.Skills <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Soft Skills.csv")

Soft.Skills_long <- Soft.Skills %>%
  gather(key = "County", value = "numSoftSkills", Crater.Region:Hopewell) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region"))) # reorders the Counties

Soft.Skills_long %>%
  group_by(Skill.Name, County) %>%
  ggplot(aes(x = reorder(Skill.Name, numSoftSkills), y = numSoftSkills, fill=County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Soft Skills in Hopewell, Prince George and Surrounding Crater Area by County",
       x = "Types of Soft Skills",
       y = "Number of Soft Skills") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))


head(Soft.Skills)


#############################################################################


Education.Levels <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Education Levels.csv")

Education.Levels_long <- Education.Levels %>%
  gather(key = "County", value = "numEducationLevels", Crater.Region:Hopewell) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region")))

Education.Levels_long %>%
  group_by(Minimum.Education.Level, County) %>%
  ggplot(aes(x = reorder(Minimum.Education.Level, numEducationLevels), y = numEducationLevels, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Minimal Education Levels and Location",
       x = "Education Level Attained",
       y = "Number of Job Ads") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))

head(Education.Levels)


##############################################################################

### Turn over by Industry

Turnover.by.Industry <- read.csv("C:/Users/John Wright/Desktop/HopwellPrinceGeorge/Turnover by Industry.csv")

Turnover.by.Industry_long <- Turnover.by.Industry %>%
  gather(key = "County", value = "numTurnoverbyIndustry", Crater.Area:Hopewell) %>%
  mutate(County = fct_relevel(County, levels = c("Hopewell","Prince.George","Crater.Region")))

Turnover.by.Industry_long %>%
  group_by(Type.of.Industry, County) %>%
  ggplot(aes(x = reorder(Type.of.Industry, numTurnoverbyIndustry), y = numTurnoverbyIndustry, fill = County)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Turnover by Industry in Hopewell, Prince George and Surrounding Crater Region by County",
       x = "Industry",
       y = "Number of Jobs available by Industry") +
  scale_fill_discrete(labels = c("Hopewell","Prince George","Crater Region"))

head(Turnover.by.Industry)

#############################################

