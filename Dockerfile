FROM ruby:3.2.2-alpine3.18
WORKDIR /app
COPY . .
RUN rm pacman.svg
CMD ruby pacman.rb && cat pacman.x3d
