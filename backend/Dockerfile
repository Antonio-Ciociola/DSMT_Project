# Build stage (optional, if you want to build inside Docker)
FROM maven:3.9-eclipse-temurin-17 AS build
WORKDIR /app

# Copy only pom.xml first to leverage Docker cache for dependencies
COPY pom.xml .
RUN mvn dependency:go-offline -B

# Now copy source code and build
COPY src ./src
COPY server.xml .
RUN mvn package -DskipTests -o

# Run stage
FROM tomcat:10.1-jdk17-temurin
RUN rm -rf /usr/local/tomcat/webapps/*

# Copy custom server.xml with thread pool configuration
COPY server.xml /usr/local/tomcat/conf/server.xml

# Copy the WAR file from build stage to Tomcat webapps
COPY --from=build /app/target/auction-system.war /usr/local/tomcat/webapps/ROOT.war

EXPOSE 8080
CMD ["catalina.sh", "run"]
