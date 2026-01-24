SWAP_FILE := /swapfile

swap-all: 
	@test -f $(SWAP_FILE) && echo "#### Swap system already configured!" || $(MAKE) swap-install

swap-install:
	@echo "#### Configuring swap in the system"

	@echo "-- Creating swap file"
	sudo touch $(SWAP_FILE)
	sudo mkswap -U clear --size 64G --file /swapfile

	@echo "-- Activating the swap file"
	sudo swapon $(SWAP_FILE)

	@echo "-- Adding swap file to /etc/fstab"
	@echo '$(SWAP_FILE) none swap defaults 0 0' | sudo tee -a /etc/fstab
	