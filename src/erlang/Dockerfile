# Use official Erlang image
FROM erlang:26-alpine

# Install build tools
RUN apk add --no-cache bash make git curl

# Set working directory
WORKDIR /app

# Copy rebar3 config first for dependency caching
COPY rebar.config .

# Fetch and compile dependencies (cached layer)
RUN rebar3 get-deps && rebar3 compile

# Copy source files and Makefile
COPY src ./src
COPY Makefile .

# Compile only the application code (dependencies already compiled)
RUN make

# Expose the HTTP port
EXPOSE 8081

# Copy and setup the Docker startup script
COPY docker_start.sh /usr/local/bin/docker_start.sh
RUN chmod +x /usr/local/bin/docker_start.sh

CMD ["docker_start.sh"]