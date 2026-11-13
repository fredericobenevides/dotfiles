AWS_PKGS := aws-cli

aws-all: aws-pkgs

aws-pkgs:
	@echo "#### Installing AWS CLI packages"
	sudo pacman -S --needed --noconfirm $(AWS_PKGS)
