FROM ghcr.io/rocker-org/devcontainer/r-ver:4.5

# Adding R packages
RUN install2.r --error -n 1 Rcpp sna network networkDynamic Matrix MASS \
  MatchIt SparseM igraph viridisLite knitr rmarkdown ape \
   RSiena RcppArmadillo

RUN apt-get update && apt-get install --no-install-recommends -y libglpk-dev

RUN installGithub.r USCCANA/netdiffuseR

# Architecture-specific variable (built in Docker BuildKit)
ARG TARGETARCH

# Install quarto-cli and the orange-book extension
RUN wget https://github.com/quarto-dev/quarto-cli/releases/download/v1.9.35/quarto-1.9.35-linux-${TARGETARCH}.deb && \
    dpkg -i quarto-1.9.35-linux-${TARGETARCH}.deb && \
    rm quarto-1.9.35-linux-${TARGETARCH}.deb && \
    quarto add --no-prompt quarto-ext/orange-book

CMD ["bash"]
