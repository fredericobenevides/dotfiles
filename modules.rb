module OS
  def OS.mac?
    (/darwin/ =~ RUBY_PLATFORM) != nil
  end

  def OS.mountain_lion?
    (/^10\.8/ =~ %x{sw_vers -productVersion}) != nil
  end
end

MODULES = [
  :homebrew, :git, :zsh, :ruby, :vim
]
