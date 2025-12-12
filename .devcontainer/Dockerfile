FROM mcr.microsoft.com/devcontainers/base:2-bookworm@sha256:23fa69fed758b7927c60061317d73baf7d66b9fca5c344e80bce3a940b229af0

ARG USERNAME=vscode

# dos2unixのインストール
RUN apt-get update && apt-get install -y --no-install-recommends \
    dos2unix \
    && rm -rf /var/lib/apt/lists/*

# sdkman + Java 21 + sbt のインストール（"vscode"ユーザーにインストールする）
USER $USERNAME
SHELL ["/bin/bash", "-c"]
ENV SDKMAN_DIR="/home/vscode/.sdkman"
RUN curl -s "https://get.sdkman.io" | bash && source "$SDKMAN_DIR/bin/sdkman-init.sh" && \
    sdk install java 21.0.9-tem && \
    sdk install sbt 1.11.6
USER root
