# okBATHTUB Oklahoma Coefficients
# Generated: 2026-05-08 17:36:07.222021
# Calibration: OWRB LMP 2000-2024, 82 lakes, 250 obs

.oklahoma_coefficients <- function() {

  message(
    'Oklahoma-specific Chl-a and Secchi coefficients applied.\n',
    'TP retention uses Walker (1996) defaults.\n',
    'Calibrated from OWRB LMP data (2000-2024, 82 lakes, 250 obs).'
  )

  list(
    tp_retention_form    = 'larsen_mercier',
    tn_settling_velocity = 10.0,

    # Ecoregion-specific Chl-a coefficients
    # log10(chla) = chla_intercept + chla_slope * log10(tp_inlake)
    chla_coefficients = list(
      'Central Great Plains' = list(
        intercept = 0.150454,
        slope     = 0.671541,
        source    = 'oklahoma_statewide',
        r_squared = 0.4422,
        n_obs     = 250,
        n_lakes   = 82
      ),
      'Central Oklahoma/Texas Plains' = list(
        intercept = 0.048470,
        slope     = 0.746172,
        source    = 'oklahoma_ecoregion',
        r_squared = 0.6138,
        n_obs     = 24,
        n_lakes   = 10
      ),
      'Cross Timbers' = list(
        intercept = 0.282294,
        slope     = 0.617121,
        source    = 'oklahoma_ecoregion',
        r_squared = 0.3913,
        n_obs     = 169,
        n_lakes   = 36
      ),
      'Ouachita Mountains' = list(
        intercept = 0.150454,
        slope     = 0.671541,
        source    = 'oklahoma_statewide',
        r_squared = 0.4422,
        n_obs     = 250,
        n_lakes   = 82
      ),
      'Ozark Highlands' = list(
        intercept = -0.168379,
        slope     = 0.802069,
        source    = 'oklahoma_ecoregion',
        r_squared = 0.6089,
        n_obs     = 20,
        n_lakes   = 14
      ),
      'Arkansas Valley' = list(
        intercept = 0.150454,
        slope     = 0.671541,
        source    = 'oklahoma_statewide',
        r_squared = 0.4422,
        n_obs     = 250,
        n_lakes   = 82
      ),
      'South Central Plains' = list(
        intercept = 0.150454,
        slope     = 0.671541,
        source    = 'oklahoma_statewide',
        r_squared = 0.4422,
        n_obs     = 250,
        n_lakes   = 82
      ),
      'Flint Hills' = list(
        intercept = 0.150454,
        slope     = 0.671541,
        source    = 'oklahoma_statewide',
        r_squared = 0.4422,
        n_obs     = 250,
        n_lakes   = 82
      )
    ),

    # Ecoregion-specific Secchi coefficients
    # log10(secchi) = secchi_intercept + secchi_slope * log10(chla)
    secchi_coefficients = list(
      'Central Great Plains' = list(
        intercept = 0.473007,
        slope     = -0.532952,
        source    = 'oklahoma_statewide',
        r_squared = 0.3636,
        n_obs     = 250,
        n_lakes   = 82
      ),
      'Central Oklahoma/Texas Plains' = list(
        intercept = 0.648907,
        slope     = -0.574305,
        source    = 'oklahoma_ecoregion',
        r_squared = 0.3945,
        n_obs     = 24,
        n_lakes   = 10
      ),
      'Cross Timbers' = list(
        intercept = 0.433422,
        slope     = -0.523546,
        source    = 'oklahoma_ecoregion',
        r_squared = 0.3589,
        n_obs     = 169,
        n_lakes   = 36
      ),
      'Ouachita Mountains' = list(
        intercept = 0.473007,
        slope     = -0.532952,
        source    = 'oklahoma_statewide',
        r_squared = 0.3636,
        n_obs     = 250,
        n_lakes   = 82
      ),
      'Ozark Highlands' = list(
        intercept = 0.473007,
        slope     = -0.532952,
        source    = 'oklahoma_statewide',
        r_squared = 0.3636,
        n_obs     = 250,
        n_lakes   = 82
      ),
      'Arkansas Valley' = list(
        intercept = 0.473007,
        slope     = -0.532952,
        source    = 'oklahoma_statewide',
        r_squared = 0.3636,
        n_obs     = 250,
        n_lakes   = 82
      ),
      'South Central Plains' = list(
        intercept = 0.473007,
        slope     = -0.532952,
        source    = 'oklahoma_statewide',
        r_squared = 0.3636,
        n_obs     = 250,
        n_lakes   = 82
      ),
      'Flint Hills' = list(
        intercept = 0.473007,
        slope     = -0.532952,
        source    = 'oklahoma_statewide',
        r_squared = 0.3636,
        n_obs     = 250,
        n_lakes   = 82
      )
    )
  )
}
