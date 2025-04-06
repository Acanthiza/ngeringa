make_mod <- function(data) {
  
  if(sum(data$cells > 0) > 4) {
  
    # stan options
    options(mc.cores = 1)
    rstan::rstan_options(auto_write = TRUE)
    
    res <- list()
    
    res$mod <- rstanarm::stan_glm(cells ~ year
                                  , data = data
                                  , family = poisson()
                                  )
    
    
    res$post_year <- bayesplot::mcmc_areas(as.matrix(res$mod)
                                           , pars = "year"
                                           , prob = 0.8
                                           )
    
    res$sign_year <- if(sign(res$mod$coefficients[[2]]) < 0) "decrease" else "increase"
    
    res$like_year <- sum(as.matrix(res$mod)[,2] * (if(res$sign_year == "decrease") 1 else -1) < 0) / length(as.matrix(res$mod)[,2])
    
    res$likelihood <- envFunc::add_likelihood(tibble::tibble(col = res$like_year), col = "col")$likelihood
    
    res$trend <- if(grepl("Very|Extremely|Virtually", res$likelihood)) paste0(res$likelihood, " ", res$sign_year) else "probably stable"
    
    res$preds <- tibble::tibble(year = seq(min(data$year), max(data$year))) |>
      tidybayes::add_predicted_draws(object = res$mod) %>%
      dplyr::ungroup()
    
    res$results <- res$preds |>
      dplyr::group_by(year) |>
      dplyr::summarise(mean = mean(.prediction)
                       , median = median(.prediction)
                       , ci90up = quantile(.prediction, probs = 0.95)
                       , ci90lo = quantile(.prediction, probs = 0.05)
                       ) |>
      dplyr::ungroup()
    
    res$plot <- ggplot() +
      geom_line(data = res$results
                , aes(year, median, col = "model")
                ) +
      geom_ribbon(data = res$results
                  , aes(x = year
                        , ymax = ci90up
                        , ymin = ci90lo
                        , fill = "90% credible interval"
                        )
                  , alpha = 0.2
                  ) +
      geom_point(data = data
                 , aes(year, cells, col = "original data")
                 ) +
      labs(y = "cells")
    
  } else res <- list(sign_year = "unknown", like_year = NA, likelihood = "Unknown", trend = "unknown"
                     , plot = {
                       
                       ggplot(data, aes(year, cells)) +
                         geom_point()
                       
                       }
                     )
  
  return(res)
  
}