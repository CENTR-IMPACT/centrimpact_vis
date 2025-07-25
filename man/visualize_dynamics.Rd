% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize_dynamics.R
\name{visualize_dynamics}
\alias{visualize_dynamics}
\title{Create Dynamics Visualizations}
\usage{
visualize_dynamics(
  dynamics_df,
  domain_df,
  project_title,
  report_date = Sys.Date(),
  colors = NULL,
  font = NULL
)
}
\arguments{
\item{dynamics_df}{Data frame of dynamics data.}

\item{domain_df}{Data frame of domain data.}

\item{project_title}{Character string specifying the project name for titles and contextual labeling.}

\item{report_date}{Date object specifying when the report was generated. Defaults to current system date.}

\item{colors}{Optional list of color definitions.}

\item{font}{Optional font specification for text rendering.}

\item{color_df}{Color scheme data frame.}

\item{font_df}{Font scheme data frame.}
}
\value{
A ggplot2 object containing the dynamics visualization.
}
\description{
This function generates comprehensive visualizations of Project Dynamics based on the CEnTR-IMPACT framework.
}
\details{
Create Dynamics Visualizations
}
