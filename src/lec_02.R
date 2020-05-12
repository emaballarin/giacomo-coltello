library(boot)
library(readr)

# Get the data directly from their web source
student_score <- readr::read_delim(
    "https://web.stanford.edu/~hastie/CASI_files/DATA/student_score.txt",
    delim = " "
)


# Set hyperparameters
dataset_rows <- nrow(student_score)
outer_replicates <- 200  # For estimates of psi C.I.
inner_replicates <- 200  # For estimate of Var for psi-estimator


# Define the statistic of interest
psi_fun <- function(data)
{
    eig <- eigen(cor(data))$values
    return (max(eig) / sum(eig))
}


# Define the statistic of interest in the form required by boot (data-indexed)
psi_boot_plain <- function(data, indices)
{
    return (psi_fun(data[indices,]))
}


# Define the statistic of interest in the form required by ci.boot (var-endowed)
psi_boot <- function(data, indices)
{
    boot_sample <- data[indices,]

    # Estimate the Variance of psi_fun(boot_sample) via bootstrap-in-bootstrap
    boot_inner <- boot::boot(data = boot_sample,
                             statistic = psi_boot_plain,
                             R = inner_replicates,
                             sim = "ordinary",
                             stype = "i")
    return (c(psi_fun(boot_sample), var(boot_inner$t)))
}


# Produce bootstrap resamples
boot_resv <- boot::boot(data = student_score,
                        statistic = psi_boot,
                        R = outer_replicates,
                        sim = "ordinary",
                        stype = "i")

# Produce confidence intervals
boot_ci <- boot::boot.ci(boot.out = boot_resv,
                         conf = 0.95,
                         type = c("basic", "stud", "perc"))

print(boot_ci)
