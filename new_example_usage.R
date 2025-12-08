## Step 1. Read data
tga <- read.csv('beech_example.csv', skip = 40)

## Step 2. Process data
p <- process(data = tga, # dataset
             init_mass = 18.96,
             temp = 'temp',
             mass_loss = 'mass_loss',
             time = 'time',
             pyrolysis_start_time = 127,
             pyrolysis_end_time = 191.5,
             temp_units = 'C')

## Step 3. Visualise data
plot(p)

## Step 3b. Visualise temperature program
plot_temp_program(tga, time_col = "time", temp_col = "temp", stage_col = "stage")

## Step 4. (Phase II) Model deconvolution
volatile_fractions <- deconvolve(p)

## Step 5. Visualise deconvolution
plot(volatile_fractions)


m <- calculate_moisture_content(p)
a <- calculate_ash_content(p)

fc <- calculate_fixed_carbon_fractions(volatile_fractions)
total <- calculate_total_fractions(volatile_fractions, fc)


















# add visual of the temp program to the GUI


# H = 29.18689
m2 <- 10.1629
m3 <- 1.5703
m6 <- 0.1811
Hvp <- 25.05
Cvp <- 41.53
Lvp <- 16.12
