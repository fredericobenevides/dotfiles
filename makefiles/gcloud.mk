GCLOUD_DIR := $(HOME)/.gcloud
GCLOUD_TMP := /tmp/google-cloud-cli-linux-x86_64.tar.gz
GCLOUD_URL := https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-cli-linux-x86_64.tar.gz

gcloud-all:
	@test -d $(GCLOUD_DIR) && echo "#### gcloud already configured!" || $(MAKE) gcloud-setup

gcloud-setup:
	@echo "#### Installing and configuring google-cloud-cli"

	@echo "-- Downloading google-cloud-cli"
	curl -o $(GCLOUD_TMP) -L $(GCLOUD_URL)

	@echo "-- Creating the directory $(GCLOUD_DIR)"
	mkdir -p $(GCLOUD_DIR)

	@echo "-- Extracting google-cloud-cli to $(GCLOUD_DIR)"
	tar -xf $(GCLOUD_TMP) -C $(GCLOUD_DIR) --strip-components=1

	@echo "-- Running install script from google-cloud-cli"
	$(GCLOUD_DIR)/install.sh --quiet --command-completion false --path-update false --usage-reporting false

	@echo "-- Cleaning up temporary file"
	rm $(GCLOUD_TMP)
