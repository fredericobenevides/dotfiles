CARGO_DIR := $(HOME)/.cargo
RUST_INIT := source $(CARGO_DIR)/env

rust-all: rust-install rust-pkgs

rust-install:
	@test -d $(CARGO_DIR) && echo "#### Rust already configured!" || $(MAKE) rust-setup

rust-setup:
	@echo "#### Installing Rust (rustup)"
	@curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path

rust-pkgs:
	@echo "#### Configuring Rust components"
	@bash -c "$(RUST_INIT) && rustup component add rust-analyzer"