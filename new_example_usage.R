## Step 1. Read data
# beech <- read.csv('beech_example.csv', skip = 40)
# don't need to read this file because it's loaded with the package

## Step 2. Process data
processed_data <- process(data = beech, # dataset
                          init_mass = 10.64,
                          temp = 'temp',
                          mass_loss = 'mass_loss',
                          time = 'time',
                          pyrolysis_start_time = 127,
                          pyrolysis_end_time = 191.5,
                          temp_units = 'C')

## Step 3. Visualise data
plot(processed_data)
## Step 3b. Visualise temperature program
plot_temp_program(tga, time_col = "time", temp_col = "temp", stage_col = "stage")

## Step 4. (Phase II) Deconvolution of pyrolysis phase
volatile_fractions <- deconvolve(processed_data)

## Step 5. Visualise deconvolution
plot(volatile_fractions)

## Step 6. (Phase III) Separate fixed carbon fractions
fc <- calculate_fixed_carbon_fractions(volatile_fractions)

moisture_fraction <- calculate_moisture_content(processed_data)
ash_fraction <- calculate_ash_content(p)

## Step 7. Calculate total fractions of the subcomponents
total <- calculate_total_fractions(volatile_fractions, fc)







# H = 29.18689
m2 <- 10.1629
m3 <- 1.5703
m6 <- 0.1811
Hvp <- 25.05
Cvp <- 41.53
Lvp <- 16.12
