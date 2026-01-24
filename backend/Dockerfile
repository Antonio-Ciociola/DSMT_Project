# Build stage (optional, if you want to build inside Docker)
FROM maven:3.9-eclipse-temurin-17 AS build
COPY . /app
WORKDIR /app
RUN mvn clean package

# Run stage
FROM tomcat:10.1-jdk17-temurin
RUN rm -rf /usr/local/tomcat/webapps/*
# Copy the WAR file from build stage to Tomcat webapps
RUN ls -R 
COPY --from=build /app/target/auction-system.war /usr/local/tomcat/webapps/ROOT.war

EXPOSE 8080
CMD ["catalina.sh", "run"]
