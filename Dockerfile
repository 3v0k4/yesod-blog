FROM 3v0k4/yesod

RUN mkdir /app
COPY . /app
WORKDIR /app
RUN stack build
