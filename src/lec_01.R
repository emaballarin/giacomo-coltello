library(readr)

# Get the data directly from their web source
student_score <- readr::read_delim(
    "https://web.stanford.edu/~hastie/CASI_files/DATA/student_score.txt",
    delim = " "
)


# Set hyperparameters
dataset_rows <- nrow(student_score)
outer_replicates <- 1000  # For estimates of psi C.I.
inner_replicates <- 100   # For estimate of Var for psi-estimator


# Define the statistic of interest
psi_fun <- function(data)
{
    eig <- eigen(cor(data))$values
    return (max(eig) / sum(eig))
}


# SE estimation via bootstrap
bootsde <- function(data, statistic, replicates)
{
    data_n <- nrow(data)
    stat_accum <- rep(0, replicates)

    for (i in 1:replicates)
    {
        local_idxs <- sample(1:data_n, data_n, replace = TRUE)
        stat_accum[i] <- statistic(data[local_idxs,])
    }

    return (sd(stat_accum))
}


# Studentized CI bootstrap estimation of a statistic
boot_studci <- function(data_ci, statistic_ci, ci_bounds, replicates_ci, replicates_ci_inner)
{
    data_n_ci <- nrow(data_ci)
    stat_center <- statistic_ci(data_ci)

    stat_accum_ci <- rep(0, replicates_ci)
    to_quantilize <- rep(0, replicates_ci)

    for (i_ci in 1:replicates_ci)
    {
        local_idxs_ci <- sample(1:data_n_ci, data_n_ci, replace = TRUE)
        bootdata <- data_ci[local_idxs_ci,]
        stat_accum_ci[i_ci] <- statistic_ci(bootdata)
        stat_sde_accum_ci <- bootsde(data = bootdata,
                                     statistic = statistic_ci,
                                     replicates = replicates_ci_inner)
        to_quantilize[i_ci] <- ((stat_accum_ci[i_ci] - stat_center)/(stat_sde_accum_ci))
    }

    my_sde <- sd(stat_accum_ci)

    my_quantiles <- quantile(to_quantilize, prob=ci_bounds, names = FALSE)

    cinterval <- c((stat_center - my_sde*my_quantiles[1]),(stat_center - my_sde*my_quantiles[2]))

    return(cinterval)
}


# Main
boot_studci(
    data_ci = student_score,
    statistic_ci = psi_fun,
    ci_bounds = c(0.975, 0.025),
    replicates_ci = outer_replicates,
    replicates_ci_inner = inner_replicates
        )
