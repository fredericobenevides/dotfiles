PYTHON_VENV := $(HOME)/.venv
PIP_BIN     := $(PYTHON_VENV)/bin/pip

PYTHON_PKGS := epc \
			   orjson \
               sexpdata \
               six \
               setuptools \
               paramiko \
               rapidfuzz

python-all: python-install python-pkgs

python-install:
	@test -d $(PYTHON_VENV) && echo "#### Python venv already configured!" || $(MAKE) python-setup

python-setup:
	@echo "#### Creating Python virtual environment"
	@python -m venv $(PYTHON_VENV)
	@echo "-- Upgrading pip inside venv"
	@$(PIP_BIN) install --upgrade pip

python-pkgs:
	@echo "#### Installing Python packages for Emacs/Tools"
	@$(PIP_BIN) install $(PYTHON_PKGS)