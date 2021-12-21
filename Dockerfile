# Using a small image of debian
FROM bitnami/minideb:latest

ARG DEBIAN_FRONTEND=noninteractive

# Install Voss II dependencies
RUN apt-get update
RUN apt-get -y install git gcc g++ doxygen flex bison gawk \
                       libz-dev tcl-dev tk-dev libc6-dev imagemagick \
                       clang libreadline-dev python3 pandoc \
                       ghc bnfc

# Get and build Voss II
RUN git clone https://github.com/TeamVoss/VossII.git
RUN make -C VossII/src install

# Remove useless stuff to decrease image stuff
# The good old rm roulette
RUN rm -rf /VossII/.git /VossII/ckt_examples /VossII/src/external \
           /VossII/tutorials
RUN apt-get -y remove git g++ doxygen flex bison gawk \
                      clang python python3 pandoc \
                      llvm-7-dev libgl1-mesa-dri libllvm7 clang-7
RUN apt-get -y autoremove

# Build bifrost and copy cephalopode files
COPY . /source_code/
RUN make -C source_code bifrost
RUN rm -f /source_code/.bashrc

COPY ./.bashrc /root/.bashrc

CMD bash
