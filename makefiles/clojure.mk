CLOJURE_PKGS := clojure \
		leiningen 

clojure-all:
	@pacman -Q clojure > /dev/null 2>&1 && echo "#### Clojure already configured!" || $(MAKE) clojure-install

clojure-install:
	@echo "#### Installing clojure packages"
	sudo pacman -S --needed --noconfirm $(CLOJURE_PKGS)
