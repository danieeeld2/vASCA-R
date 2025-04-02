# Alpine Linux with Octave, R and Go
# This Dockerfile is intended to be used for testing purposes only
# It installs some basic tools, Octave, R and Go, and runs the tests of the project
FROM alpine:latest

# Install some basic tools
RUN apk add --no-cache \
    bash \
    curl \
    git \
    make \
    gcc \
    g++ \
    libc-dev

# Install Octave and gnuplot for plotting support
RUN apk add --no-cache octave gnuplot

# Install R
RUN apk add --no-cache R R-dev R-doc

# Install Go
RUN apk add --no-cache go

# Configure Go
ENV GOPATH=/go
ENV PATH=$PATH:/go/bin

# Copy dependencies into the container (no needed by the moment)

# Install Go dependencies (if any)
RUN go mod init tests
RUN go mod tidy

# Run the tests
CMD ["go", "test", "./tests/..."]