K8S_PKGS := kind \
             kubectl

k8s-all: k8s-install k8s-pkgs

k8s-install:
	@echo "#### Kubernetes packages ready to install"

k8s-pkgs:
	@echo "#### Installing Kubernetes packages"
	sudo pacman -S --needed --noconfirm $(K8S_PKGS)
