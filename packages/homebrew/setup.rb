def setup_homebrew
  if OS.mac?
    system %(ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)")

    username = `whoami`.chomp
    puts "\n==> Changing the permission folder of /usr/local to the owner #{username}"
    %x{sudo chown -R #{username} /usr/local}
  end
end