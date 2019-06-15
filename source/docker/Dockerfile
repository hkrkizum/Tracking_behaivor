# Base image
FROM hkrkizum/bioinfo_base:latest

SHELL ["/bin/bash", "-c"]

# Set up python
RUN git clone https://github.com/yyuu/pyenv.git /home/biodocker/.pyenv && \
    echo 'export PYENV_ROOT="$HOME/.pyenv"' >> /home/biodocker/.bashrc && \
    echo 'export PATH="$PYENV_ROOT/bin:$PATH"' >> /home/biodocker/.bashrc && \
    echo 'eval "$(pyenv init -)"' >> /home/biodocker/.bashrc && \
    source /home/biodocker/.bashrc

RUN /home/biodocker/.pyenv/bin/pyenv install anaconda3-2019.03 && \
    /home/biodocker/.pyenv/bin/pyenv global anaconda3-2019.03 && \
    /home/biodocker/.pyenv/bin/pyenv rehash

RUN /home/biodocker/.pyenv/shims/conda install -y -c conda-forge opencv

CMD ["/bin/bash", "--login"]