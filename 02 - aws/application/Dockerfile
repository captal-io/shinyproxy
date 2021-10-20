FROM openjdk:8-jre

RUN mkdir -p /opt/shinyproxy/
RUN wget https://www.shinyproxy.io/downloads/shinyproxy-2.4.3.jar -O /opt/shinyproxy/shinyproxy.jar

COPY application.yml /opt/shinyproxy/application.yml
COPY templates/ /opt/shinyproxy/templates/

WORKDIR /opt/shinyproxy/
CMD ["java", "-jar", "/opt/shinyproxy/shinyproxy.jar"]