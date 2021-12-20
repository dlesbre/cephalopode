FROM debian:latest

ARG DEBIAN_FRONTEND=noninteractive

# Install Voss II dependencies
RUN apt-get update
RUN apt-get -y install git gcc g++ doxygen flex bison gawk \
                       libz-dev tcl-dev tk tk-dev libc6-dev imagemagick \
                       clang libreadline-dev python3 imagemagick pandoc \
                       ghc bnfc

# Get and build Voss II
RUN git clone https://github.com/TeamVoss/VossII.git
RUN make -C VossII/src install

# Build bifrost and copy cephalopode files
COPY . cephalopode/
RUN make -C cephalopode bifrost

CMD bash
