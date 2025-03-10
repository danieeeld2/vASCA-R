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

# Install Octave
RUN apk add --no-cache octave

# Install R
RUN apk add --no-cache R R-dev R-doc

# Install Go
RUN apk add --no-cache go

# Configure Go
ENV GOPATH=/go
ENV PATH=$PATH:/go/bin

# Copy the project into the container
COPY . /app
WORKDIR /app

# Install Go dependencies (if any)
RUN go mod init proyecto
RUN go mod tidy

# Añadido linea de prueba para ver si se ejecuta el test

# Run the tests
CMD ["go", "test", "./tests/..."]