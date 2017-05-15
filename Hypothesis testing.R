ht <- function(y_bar, mu, sigma, n, alpha, sides) {
  curve(dnorm(x, mean=mu, sd=sigma), from=mu-2*sigma, to=mu+2*sigma)
  abline(v=y_bar, col='blue')
  if (sides == 1 | sides == -1) { # one-sided 
    zscore = sides * qnorm(1-alpha)
    
    # draw cutoff line
    cutoff = zscore * sigma/sqrt(n) + mu
    abline(v=cutoff, col='red')
    axis(3, at=cutoff, labels=round(cutoff, 2))
    
    # calculate z for Y_bar
    z_y_bar = (y_bar - mu)/(sigma/sqrt(n))
    
    # print report
    if (sides == -1) {
      print('Y_bar zscore must be less than or equal to the cutoff zscore to reject H_0.')
      if (z_y_bar <= zscore) {
        print('Reject null hypothesis.')
      } else {
        print('Accept null hypothesis.')
      }
    } else {
      print('Y_bar zscore must be greater than or equal to the cutoff zscore to reject H_0.')
      if (z_y_bar >= zscore) {
        print('Reject null hypothesis.')
      } else {
        print('Accept null hypothesis.')
      }
    }
    print(paste('Cutoff zscore =', round(zscore, 2)))
    print(paste("Y_bar zscore =", round(z_y_bar, 2)))
    print(paste('The p-value is', round(pnorm(z_y_bar, lower.tail=sides<0), 2)))
    
  } else if (sides == 0) { # two-sided
    
    # calculate cutoff zscore
    zscore = qnorm(1-alpha/2)

    # right cutoff line
    right_cutoff = zscore * sigma/sqrt(n) + mu
    axis(3, at=right_cutoff, labels=round(right_cutoff, 2))
    abline(v=right_cutoff, col='red')
    
    # left cutoff line
    left_cutoff = -zscore * sigma/sqrt(n) + mu
    axis(3, at=left_cutoff, labels=round(left_cutoff, 2))
    abline(v=left_cutoff, col='red')
    
    # calculate z for Y_bar
    z_y_bar = (y_bar - mu)/(sigma/sqrt(n))
    
    # print report
    print('Y_bar zscore must be at or outside the cutoffs to reject H_0.')
    if (z_y_bar > -zscore & z_y_bar < zscore) {
      print('Accept null hypothesis.')
    } else {
      print('Reject null hypothesis.')
    }
    print(paste("Cutoff zscore = ±", round(zscore, 2), sep=''))
    print(paste("Y_bar zscore =", round(z_y_bar, 2)))
    print(paste('The p-value is', round(2*(1 - pnorm(z_y_bar)), 3)))
  }
}
