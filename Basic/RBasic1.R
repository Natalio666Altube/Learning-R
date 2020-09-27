my.name <- readline(prompt="Enter name: ")
my.age <- readline(prompt="Enter age: ")
r.installation <- readline(prompt = "R.installation: ")
# convert character into integer
my.age <- as.integer(my.age)
print(paste("Hi,", my.name, my.age, r.installation))
