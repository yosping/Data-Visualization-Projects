############
#
# Here's a file with my job application timeline data, and a way to 
# visualize those data.
# Most recent revision 2023-05-18
#
############
#
# Start by building the variables
#
###
#
# An ID number for each job application
#
###
id <- c(01:15)
###
#
# Application date
#
###
applied <- as.Date(c('2022-10-15','2023-01-04','2023-02-13','2023-02-14',
                     '2023-02-27','2023-03-14','2023-03-20','2023-03-28',
                     '2023-04-14','2023-04-24','2023-04-25','2023-05-03',
                     '2023-05-04','2023-05-10','2023-05-11'))
###
#
# Date of first interview (if any)
#
###
FirstInterview <- as.Date(c('2022-12-15',NA,NA,'2023-03-08','2023-04-04',
                            '2023-04-06',NA,NA,NA,NA,NA,NA,NA,NA,NA))
###
#
# Date of second interview
#
###
SecondInterview <- as.Date(c('2023-01-25',NA,NA,NA,'2023-04-17',NA,NA,NA,NA,NA,
                             NA,NA,NA,NA,NA))
###
#
# Date of third interview (not many of these)
#
###
ThirdInterview <- as.Date(c('2023-02-10',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                            NA))
###
#
# And the date of rejection.
#
###
Rejection <- as.Date(c('2023-03-03','2023-02-15','2023-04-21','2023-05-12',
                       '2023-05-09','2023-04-10',NA,NA,NA,'2023-04-25',NA,NA,NA,
                       NA,NA))
#####
#
# Put them together into a data frame.
#
#####
dat <- data.frame(id,applied,FirstInterview,SecondInterview,ThirdInterview,
                  Rejection)
#####
#
# I need to structure the data differently. I'd like to have columns of id,
# start, end, type. The types should be 0 = applied, 1 = first interview,
# 2 = second interview, 3 = third interview, 7 = ghosted, 8 = rejected, 
# 9 = accepted
#
#####
#
# Here are the new id numbers. For some of them with only one entry, they will 
# need to run to TODAY, not the rejection data, because so far I haven't been
# rejected.
#
id <- c(1,1,1,1,2,3,4,4,5,5,5,6,6,7,8,9,10,11,12,13,14,15)
#
###
#
# These are the start dates for each event.
#
EventStartDate <- as.Date(c('2022-10-15','2022-12-15','2023-01-25','2023-02-10',
                            '2023-01-04','2023-02-13','2023-02-14','2023-03-08',
                            '2023-02-27','2023-04-04','2023-04-17','2023-03-14',
                            '2023-04-06','2023-03-20','2023-03-28','2023-04-14',
                            '2023-04-24','2023-04-25','2023-05-03','2023-05-04',
                            '2023-05-10','2023-05-11'))
#
###
#
# These are the end dates for each event.
#
EventEndDate <- as.Date(c('2022-12-15','2023-01-25','2023-02-10','2023-03-03',
                          '2023-02-15','2023-04-21','2023-03-08','2023-05-12',
                          '2023-04-04','2023-04-17','2023-05-09','2023-04-06',
                          '2023-04-10',as.character(Sys.Date()),
                          as.character(Sys.Date()),as.character(Sys.Date()),
                          '2023-04-25',as.character(Sys.Date()),
                          as.character(Sys.Date()),as.character(Sys.Date()),
                          as.character(Sys.Date()),as.character(Sys.Date())))
#
###
#
# And these are the event types
#
EventType <- c(0,1,2,3,0,0,0,1,0,1,2,0,1,0,0,0,0,0,0,0,0,0)
#
# Force them to be Factor data
#
EventType.f <- factor(EventType, labels = c('Applied','First Interview','Second Interview','Third Interview'))
#
###
#
# Let's make those into a new data frame.
#
dat2 <- data.frame(id,EventStartDate,EventEndDate,EventType.f)
#
###
#
# Calculate the number of days each state lasts.
#
dat2$n_days <- with(dat2,difftime(EventEndDate,EventStartDate, units='days'))
#
###
#
# Make a new data frame with 1 row for each event day
#
row_rep <- unlist(mapply(rep, 1:nrow(dat2),  dat2$n_days))
dat3 <- dat2[row_rep,]
#
###
#
# Add a column containing each discrete event day
#
dat3$t_plus <- unlist(mapply(seq, 1,  dat2$n_days)) - 1
dat3$EventDay <- with(dat3, as.Date(EventStartDate) + t_plus)
#
#####
#
# Now we get to make the figure.
#
library(ggplot2)
#
# Using filled squares as the point style will make solid horizontal bars
#
figure1 <- ggplot(dat3, aes(y=factor(id), x=EventDay, color=EventType.f)) +
  geom_point(shape=15)
#
# Below is code specifying the many options I like for this figure. These
# include customs labels, inverting the y-axis, custom tick marks corresponding
# to the start of each month in the figure, and color-blind-safe colors.
#
figure1  + 
  labs(title='Job Search Timeline', y='Job Applied For', x='Date') +
  theme_bw() + 
  scale_y_discrete(limits=rev) +
  theme(legend.position='bottom') + 
  scale_fill_discrete("Applied",'First Interview',
                      'Second Interview','Third Interview') +
  scale_x_date(breaks = c(as.Date('2022-10-01'),as.Date('2022-11-01'),
                          as.Date('2022-12-01'),as.Date('2023-01-01'),
                          as.Date('2023-02-01'),as.Date('2023-03-01'),
                          as.Date('2023-04-01'),as.Date('2023-05-01'),
                          as.Date('2023-06-01'))) +
  labs(color=guide_legend(title="Job Search Status")) +
  scale_colour_brewer(palette = "PRGn")
#