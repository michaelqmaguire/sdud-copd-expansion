#-----------------------------------------------------------------------------------------------------------------#
#                                                                                                                 #
# PROJECT: SDUD COPD MEDICAID EXPANSION				                                                                    #
# AUTHOR: MICHAEL MAGUIRE, MS, DMA II                                                                             #
# INSTITUTION: UNIVERSITY OF FLORIDA, COLLEGE OF PHARMACY                                                         #
# DEPARTMENT: PHARMACEUTICAL OUTCOMES AND POLICY                                                                  #
# SUPERVISORS: AMIE GOODIN, PHD, MPP | JUAN HINCAPIE-CASTILLO, PHARMD, PHD, MS                                    #
# SCRIPT: 03_make-plots.R                                                                			                    #
#                                                                                                                 #
#-----------------------------------------------------------------------------------------------------------------#

library(hrbrthemes)
library(ggplot2)
library(viridis)

# Plot number of prescriptions by year - halfyear

rxYearHalfyear <-
  final %>%
    filter(suppression == "F") %>%
    mutate(yearHalfYear = paste0(year, "-", halfyear)) %>%
      ggplot() +
        geom_col(
          mapping = aes(
            x = yearHalfYear,
            y = totalRX
          ),
          fill = "forestgreen"
        ) +
        theme_ipsum_rc() +
        ggtitle(
          "Total Number of COPD Prescriptions by Year, and Half-Year, 2011 - 2016"
        )

# Plot Generic Name, Number of Prescriptions by Year and Half-Year.

rxGenericYearHalfYear <- 
  finalRx %>%
    filter(suppression == "F") %>%
    mutate(yearHalfYear = paste0(year, "-", halfyear)) %>%
    ggplot() +
      geom_col(
        mapping = aes(
          x = yearHalfYear,
          y = totalRX,
          fill = gennme
        )
      ) +
      theme_ipsum_rc() +
      ggtitle(
        "Total Number of COPD Prescriptions by Generic Name, Year, and Half-Year, 2011 - 2016"
      ) +
      scale_fill_viridis_d()

# Plot Generic Name, Number of Prescriptions, Year, Half-Year, and Treatment Group

rxGenericYearHalfYearTreatment <-
  finalRx %>%
    filter(suppression == "F") %>%
    mutate(yearHalfYear = paste0(year, "-", halfyear)) %>%
      ggplot() +
        geom_col(
          mapping = aes(
            x = yearHalfYear,
            y = totalRX,
            fill = gennme
          )
        ) +
        facet_wrap(~ group) +
        theme_ipsum_rc() +
        scale_y_continuous(labels = scales::comma) +
        ggtitle(
          "Total Number of COPD Prescriptions by Generic Name, Year, Half-Year and Treatment Group, 2011 - 2016"
        ) +
        scale_fill_viridis_d()


# Plot number of enrollees by year - halfyear

enrolleesYearHalfYear <-
  final %>%
    filter(suppression == "F") %>%
    mutate(yearHalfYear = paste0(year, "-", halfyear)) %>%
      ggplot() +
        geom_col(
          mapping = aes(
            x = yearHalfYear,
            y = chipMedicaidEnroll
          ),
          fill = "forestgreen"
        ) +
        theme_ipsum_rc() +
        ggtitle(
          "Total Number of Enrollees by Year and Half-Year, 2011 - 2016"
        ) + 
        scale_y_continuous(
          labels = scales::comma
        )

# Plot number of enrollees by year - halfyear, and treatment group.

enrolleesYearHalfYearTreatment <-
  final %>%
    filter(suppression == "F") %>%
    mutate(
      yearHalfYear = paste0(year, "-", halfyear),
      group = factor(group, levels = c(0, 1), labels = c("Control", "Treatment"))
      ) %>%
      ggplot() +
        geom_col(
          mapping = aes(
            x = yearHalfYear,
            y = chipMedicaidEnroll,
            fill = group
          )
        ) +
        facet_wrap(~ group) +
        scale_fill_viridis_d() +
        theme_ipsum_rc() +
        ggtitle(
          "Total Number of Enrollees by Year, Half-Year, and Treatment Group, 2011 - 2016"
        ) + 
        scale_y_continuous(
          labels = scales::comma
        )

# Plot number of prescriptions by year-halfyear and state.

rxYearHalfYearState <- 
  final %>%
    filter(suppression == "F") %>%
    mutate(yearHalfYear = paste0(year, "-", halfyear)) %>%
      ggplot() +
        geom_col(
          mapping = aes(
            x    = yearHalfYear,
            y    = totalRX,
            fill = state
          ),
          color = "black"
        ) +
        facet_wrap(~ state) +
        scale_fill_viridis_d() +
        theme_ipsum_rc() +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5),
        ) +
        ggtitle(
          "Total Number of COPD Prescriptions by Year, Half-Year, and State, 2011 - 2016"
        )

# Plot number of enrollees by year-halfyear and state.

enrolleesYearHalfYearState <- 
  final %>%
    filter(suppression == "F") %>%
    mutate(yearHalfYear = paste0(year, "-", halfyear)) %>%
      ggplot() +
        geom_col(
          mapping = aes(
            x = yearHalfYear,
            y = chipMedicaidEnroll,
            fill = state
          ),
          color = "black",
          alpha = 0.85
        ) +
        facet_wrap(~ state) +
        scale_fill_viridis_d() +
        theme_ipsum_rc() +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5),
        ) +
        scale_y_continuous(
          labels = scales::comma
        ) +
        ggtitle(
          "Total Number of Enrollees by Year, Half-Year, and State, 2011 - 2016"
        )

# Plot number of prescriptions by year-halfyear and state, filled by treatment group.

rxYearHalfYearStateTreatment <-
  final %>%
    filter(suppression == "F") %>%
    mutate(yearHalfYear = paste0(year, "-", halfyear)) %>%
      ggplot() +
        geom_col(
          mapping = aes(
            x = yearHalfYear,
            y = totalRX,
            fill = factor(x = group, levels = c(0, 1), labels = c("Control", "Treatment"))
          ),
          color = "black",
          alpha = 0.85
        ) +
          facet_wrap(~ state) +
          scale_fill_viridis_d() +
          theme_ipsum_rc() +
          labs(fill = "Group") +
          theme(
            axis.text.x = element_text(angle = 90, vjust = 0.5)
          ) +
          ggtitle(
            "Total Number of COPD Prescriptions by Year, Half-Year, State, and Treatment Group, 2011 - 2016"
          )

# Plot prescription rate per 100 by year-halfyear.

rxRateYearHalfyear <- 
  final %>%
    filter(suppression == "F") %>%
    mutate(yearHalfYear = paste0(year, "-", halfyear)) %>%
      ggplot() +
        geom_col(
          mapping = aes(
            x = yearHalfYear,
            y = (totalRX / chipMedicaidEnroll) * 100
          ),
          fill = "forestgreen"
        ) +
          theme_ipsum_rc() +
          ggtitle(
            "COPD Prescription Rate per 100 Medicaid Enrollees, 2011 - 2016"
          )

# Plot prescription rate per 100 by year-halfyear and state.

rxRateYearHalfYearState <-
  final %>%
    filter(suppression == "F") %>%
    mutate(yearHalfYear = paste0(year, "-", halfyear)) %>%
      ggplot() +
        geom_col(
          mapping = aes(
            x = yearHalfYear,
            y = (totalRX / chipMedicaidEnroll) * 100,
            fill = state
          ),
          color = "black",
          alpha = 0.85
        ) +
          facet_wrap(~ state) +
          scale_fill_viridis_d() +
          theme_ipsum_rc() +
          theme(
            legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = 0.5)
          ) +
          ggtitle(
            "Prescription Rate per 100 Medicaid Enrollees by Year, Half-Year, and State, 2011 - 2016"
          )

# Plot prescription rate per 100 by year-halfyear, state, and treatment group.

rxRateYearHalfYearStateTreatment <- 
  final %>%
    filter(suppression == "F") %>%
    mutate(yearHalfYear = paste0(year, "-", halfyear)) %>%
      ggplot() +
        geom_col(
          mapping = aes(
            x = yearHalfYear,
            y = (totalRX / chipMedicaidEnroll) * 100,
            fill = factor(x = group, levels = c(0, 1), labels = c("Control", "Treatment")),
          ),
          color = "black",
          alpha = 0.85
        ) +
        facet_wrap(~ state) +
        scale_fill_viridis_d() +
        theme_ipsum_rc() +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5)
        ) +
        labs(
          fill = "Group"
        ) +
        ggtitle(
          "Prescription Rate per 100 Medicaid Enrollees by Year, Half-Year, and State, 2011 - 2016"
        )

## Output everything into single PDF.

Cairo::CairoPDF(file = "./plots/01_all-plots.pdf", onefile = TRUE, width = 20, height = 12)
rxYearHalfyear
rxGenericYearHalfYear 
rxGenericYearHalfYearTreatment
enrolleesYearHalfYear
enrolleesYearHalfYearTreatment
rxYearHalfYearState 
enrolleesYearHalfYearState 
rxYearHalfYearStateTreatment
rxRateYearHalfyear 
rxRateYearHalfYearState
rxRateYearHalfYearStateTreatment 
dev.off()
