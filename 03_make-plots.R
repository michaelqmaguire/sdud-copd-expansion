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

rxYearhalfyear <-
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
            "Total Number of Prescriptions by Year-halfyear"
          )

# Plot number of enrollees by year - halfyear

enrolleesYearhalfyear <-
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
              "Total Number of Enrollees by Year-halfyear"
            ) + 
            scale_y_continuous(
              labels = scales::comma
        )

# Plot number of prescriptions by year-halfyear and state.

rxYearhalfyearState <- 
  final %>%
    filter(suppression == "F") %>%
    mutate(yearHalfYear = paste0(year, "-", halfyear)) %>%
      ggplot() +
        geom_col(
          mapping = aes(
            x = yearHalfYear,
            y = totalRX,
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
            "Total Number of COPD Prescriptions by Year-halfyear and State"
          )

# Plot number of enrollees by year-halfyear and state.

enrolleesYearhalfyearState <- 
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
          facet_wrap(
            ~ state
          ) +
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
            "Total Number of Enrollees by Year-halfyear and State"
          )

# Plot number of prescriptions by year-halfyear and state, filled by treatment group.

rxYearhalfyearStateTreatment <-
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
            "Total Number of COPD Prescriptions by Year-halfyear, State, and Treatment Group"
          )

# Plot prescription rate per 100,000 by year-halfyear.

rxRateYearhalfyear <- 
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
            "COPD Prescription Rate per 100 Medicaid Enrollees"
          )

# Plot prescription rate per 100 by year-halfyear and state.

rxRateYearhalfyearState <-
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
            "Prescription Rate per 100 Medicaid Enrollees by Year-halfyear and State"
          )

# Plot prescription rate per 100 by year-halfyear, state, and treatment group.

rxRateYearhalfyearStateTreatment <- 
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
            "Prescription Rate per 100 Medicaid Enrollees by Year-halfyear and State"
          )

Cairo::CairoPDF(file = "./plots/01_all-plots.pdf", onefile = TRUE, width = 20, height = 12)
rxYearhalfyear
rxYearhalfyearState
rxYearhalfyearStateTreatment
enrolleesYearhalfyear
enrolleesYearhalfyearState
rxRateYearhalfyear
rxRateYearhalfyearState
rxRateYearhalfyearStateTreatment
dev.off()

