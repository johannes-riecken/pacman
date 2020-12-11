FROM ruby:alpine
WORKDIR /app
COPY . .
RUN rm pacman.svg
CMD ruby pacman.rb && cat pacman.x3d
