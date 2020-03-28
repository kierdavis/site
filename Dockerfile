# version must match the snapshot pinned in stack.yaml
FROM haskell:8.6.5 AS build
RUN stack update

WORKDIR /build
ADD LICENSE /build/LICENSE
ADD site.cabal /build/site.cabal
ADD src /build/src
ADD stack.yaml /build/stack.yaml
ADD stack.yaml.lock /build/stack.yaml.lock
RUN stack build

RUN apt update -y
RUN apt install -y locales
RUN localedef -i en_US -f UTF-8 en_US.UTF-8
ENV LANG=en_US.UTF-8

ADD keybase.txt /build/keybase.txt
ADD pages /build/pages
ADD static /build/static
ADD templates /build/templates
RUN stack exec site build

FROM nginx
COPY --from=build /build/_site /content
ADD docker-support/nginx.default.conf /etc/nginx/conf.d/default.conf
