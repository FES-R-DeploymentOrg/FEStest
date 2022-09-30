# bring the base image from dockerhub
FROM openjdk:11

# make directory in the opt directory with the name of shinyproxy and allow parent directories as needed
RUN mkdir -p /opt/shinyproxy
#copy the shinyproxy.jar file from the working directory to the mentioned directory
COPY shinyproxy*.jar /opt/shinyproxy/shinyproxy.jar
#copy the application.yml file from the working directory to the mentioned directory
COPY application.yml /opt/shinyproxy/application.yml

#the following is the working directory of our container and image
WORKDIR /opt/shinyproxy

#finally execute the following commands to unzip the .jar file using -jar option
CMD ["java","-jar", "/opt/shinyproxy/shinyproxy.jar"]