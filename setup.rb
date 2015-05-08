$LOAD_PATH << File.join(File.dirname(__FILE__))

require 'file'

Dir.glob('packages/**/*.rb') do |package|
  require package
end

require 'modules'

MODULES.each do |m|
  puts "\n** Running the module #{m}"
  Kernel.send "setup_#{m}"
end
