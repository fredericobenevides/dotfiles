$:<< File.join(File.dirname(__FILE__))

require 'file'

Dir.glob("packages/**/*.rb") do |package|
  require package
end

require 'modules'

username = `whoami`

if OS.mac?
  system %(ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)") if OS.mac?
  puts "\n==> Changing the permission folder of /usr/local to the owner #{%x{whoami}}"
  %x{sudo chown -R `whoami` /usr/local}
end

MODULES.each do |m|
  puts "\n** Running the module #{m}"
  Kernel.send "setup_#{m}"
end