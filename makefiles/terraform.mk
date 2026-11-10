TERRAFORM_PKGS := terraform

terraform-all: terraform-pkgs

terraform-pkgs:
	@echo "#### Installing Terraform packages"
	sudo pacman -S --needed --noconfirm $(TERRAFORM_PKGS)
